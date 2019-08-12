module Utils exposing
  ( HttpError (..)
  , zip, at, find, findIdx, set, groupBy, transpose
  , orElse, filter
  , httpQuery, decodeHttpQuery, decodeUrlPath
  , matchIdx, strOrEmpty, optField, primitiveStrDecoder, emptyEncoder, noBreakSpace
  , flip, curry, uncurry, httpErrorToString, eqElCount
  , searchParams, toList, styles
  , do, domsg
  , expectJson, expectString, badHttpBody
  )


{-| Various useful utility methods.

@docs at, httpQuery, matchIdx, noBreakSpace, optField, emptyEncoder
      orElse, strOrEmpty, zip, flip, curry, uncurry, httpErrorToString
-}


import Http
import Dict exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Url.Builder as UB
import Url
import Html
import Html.Attributes as Attributes
import Task

import Debug exposing (log, toString)


type HttpError
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus Http.Metadata String
  | BadBody Http.Metadata String String


{-| Zip together to lists. If lists are various size resulting list size
equals to shortest one. This is reverse operation to `List.unzip` function from
core package.
-}
zip: List a -> List b -> List (a, b)
zip list1 list2 =
  let
    z l1 l2 r =
      case (l1, l2) of
        (e1 :: t1, e2 :: t2) -> z t1 t2 <| (e1, e2) :: r

        _ -> List.reverse r
  in
    z list1 list2 []


{-| Finds list element at specified index.
-}
at: Int -> List a -> Maybe a
at idx l =
  if idx == 0 then List.head l
  else List.tail l |> Maybe.andThen (at (idx - 1))


{-| Find first element matching condition -}
find: (a -> Bool) -> List a -> Maybe a
find cond list =
  case list of
    el :: tail ->
      if cond el then Just el else find cond tail

    [] -> Nothing


{-| Find first element index matching condition -}
findIdx: (a -> Bool) -> List a -> Maybe Int
findIdx cond list =
  let
    f idx l =
      case l of
        el :: tail ->
          if cond el then Just idx else f (idx + 1) tail

        [] -> Nothing
  in
    f 0 list


{-| Sets element at specified index. If index is out of range list remains unaltered. -}
set: Int -> a -> List a -> List a
set idx el list =
  List.foldl
    (\e (r, i) -> if i == idx then (el :: r, i + 1) else (e :: r, i + 1))
    ([], 0)
    list |>
  Tuple.first |>
  List.reverse


{-| Partitions list into a dict of lists according to some discriminator function.
-}
groupBy: (a -> comparable) -> List a -> Dict comparable (List a)
groupBy f l =
  List.foldl
    (\a r ->
      f a |>
      (\b ->
        Dict.get b r |>
        Maybe.map (\pl -> Dict.insert b (a :: pl) r) |>
        Maybe.withDefault (Dict.insert b [ a ] r)
      )
    )
    Dict.empty
    l |>
  Dict.map (\k x -> List.reverse x)


{-| Transposes list. -}
transpose: List (List a) -> List (List a)
transpose listOfLists =
  List.foldr
    ( List.map2 (::) )
    ( List.head listOfLists |>
      Maybe.map (\l -> List.repeat (List.length l) []) |>
      Maybe.withDefault [[]]
    )
    listOfLists


{-| Returns first `Just` value `Maybe`
-}
orElse: Maybe a -> Maybe a -> Maybe a
orElse mb2 mb1 = if mb1 == Nothing then mb2 else mb1


filter: (a -> Bool) -> Maybe a -> Maybe a
filter cond mb =
  mb |>
  Maybe.andThen (\a -> if cond a then Just a else Nothing)


{-| Forms http query string from key value list.
-}
httpQuery: List (String, String) -> String
httpQuery params =
  params |>
  List.map (uncurry UB.string) |>
  UB.toQuery


decodeHttpQuery: String -> List (String, String)
decodeHttpQuery query =
  (if String.startsWith "?" query then String.dropLeft 1 query else query) |>
  String.split "&" |>
  List.map (String.split "=") |>
  List.concatMap
    (\pv -> case pv of
      [] -> []

      [ n ] -> if String.isEmpty n then [] else [ (n, "") ]

      n :: v :: _ ->
        [ ( Url.percentDecode n |> Maybe.withDefault ""
          , Url.percentDecode v |> Maybe.withDefault ""
          )
        ]
    )


decodeUrlPath: String -> List String
decodeUrlPath =
  String.split "/" >>
  List.filter ((/=) "") >>
  List.map (Url.percentDecode >> Maybe.withDefault "")


{-| Useful for &nbsp; characters in html.
-}
noBreakSpace: String
noBreakSpace = String.fromChar '\u{00A0}'


{-| Finds index of element most closely matching pattern.
   Matching priority:
     1. Exact match
     2. Exact case insensitive without latvian diacritic match
     3. Starts with match
     4. Starts with case insensitive without latvian diacritic match
-}
matchIdx: String -> List String -> Maybe Int
matchIdx pattern list =
  let
    stringMatch string =
      let
        charMap =
          Dict.fromList
            [ ('ā', 'a')
            , ('č', 'c')
            , ('ē', 'e')
            , ('ģ', 'g')
            , ('ī', 'i')
            , ('ķ', 'k')
            , ('ļ', 'l')
            , ('ņ', 'n')
            , ('š', 's')
            , ('ū', 'u')
            , ('ž', 'z')
            ]

        toLowerInsensitive str =
          String.toLower str |> String.map (\c -> Dict.get c charMap |> Maybe.withDefault c)

        insensitivePattern = toLowerInsensitive pattern

        insensitiveString = toLowerInsensitive string

        partialMatch s1 s2 c =
          String.uncons s1 |>
          Maybe.andThen
            (\(ch1, t1) ->
              String.uncons s2 |>
              Maybe.map
                (\(ch2, t2) ->
                  if ch1 == ch2 then partialMatch t1 t2 (c + 1) else c
                )
            ) |>
          Maybe.withDefault c
      in
        if pattern == string then
          (0, 0)
        else if insensitivePattern == insensitiveString then
          (1, 0)
        else case partialMatch pattern string 0 of
          0 ->
            case partialMatch insensitivePattern insensitiveString 0 of
              0 -> (5, 0)

              x -> (4, String.length string - x)

          x -> (3, String.length string - x)
  in
    list |>
    List.indexedMap Tuple.pair |>
    List.map (\(i, s) -> (i, stringMatch s)) |>
    List.filter (\(_, (r, _)) -> not <| r == 5) |>
    List.sortBy (\(_, r) -> r) |>
    List.head |>
    Maybe.map Tuple.first


{-| `JD.oneOf [ JD.string, JD.null "" ]`
-}
strOrEmpty: JD.Decoder String
strOrEmpty =
  JD.oneOf [ JD.string, JD.null "" ]


{-| `JD.maybe (JD.field name decoder)`
-}
optField: String -> JD.Decoder a -> JD.Decoder (Maybe a)
optField name decoder =
  JD.maybe (JD.field name decoder)


primitiveStrDecoder: JD.Decoder String
primitiveStrDecoder =
  JD.oneOf
    [ JD.string
    , JD.float |> JD.map String.fromFloat
    , JD.null ""
    , JD.bool |> JD.map toString
    ]


{-| Always returns empty json object {} -}
emptyEncoder: value -> JE.Value
emptyEncoder =
  (always <| JE.object [])


{-| Flips first two arguments of a function. Removed from elm 0.19
-}
flip: (a -> b -> c) -> (b -> a -> c)
flip f b a =
  f a b


{-| Change how arguments are passed to a function.
This splits paired arguments into two separate arguments.
-}
curry : ((a,b) -> c) -> a -> b -> c
curry f a b =
  f (a,b)


{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) =
  f a b


{-| Converts Http Error to string and logs full error using `Debug` module
-}
httpErrorToString: HttpError -> String
httpErrorToString err =
  case log "Http error occurred" err of
    BadUrl msg -> msg

    Timeout -> "Timeout occurred"

    NetworkError -> "Network error occurred"

    BadStatus _ body -> body

    BadBody _ body msg -> msg


eqElCount: List a -> List a -> Int
eqElCount list1 list2 =
  let
    x l1 l2 c =
      case (l1, l2) of
        (e1 :: t1, e2 :: t2) ->
          if e1 == e2 then
            x t1 t2 (c + 1)
          else c

        _ -> c
  in
    x list1 list2 0


searchParams: List String -> List (a -> Maybe String) -> a -> List (String, String)
searchParams names getters params =
  List.map2 (\name getter -> ( name, getter params )) names getters |>
  List.concatMap (\(n, mv) -> Maybe.map (\v -> [(n, v)]) mv |> Maybe.withDefault [])


toList: List (a -> String) -> a -> List String
toList getters param =
  List.map (\getter -> getter param) getters


styles: List (String, String) -> List (Html.Attribute msg)
styles list =
  list |> List.map (\(k, v) -> Attributes.style k v)


do: (a -> msg) -> a -> Cmd msg
do toMsg =
  Task.succeed >> Task.perform toMsg


domsg: msg -> Cmd msg
domsg =
  do identity


expectJson: (Result HttpError a -> msg) -> JD.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
  let
    httpdec metadata resp =
      JD.decodeString decoder resp |>
      Result.mapError
        (BadBody metadata resp << JD.errorToString)
  in
    Http.expectStringResponse toMsg <| httpResult httpdec


expectString: (Result HttpError String -> msg) -> Http.Expect msg
expectString toMsg =
  Http.expectStringResponse toMsg <| httpResult (\_ resp -> Ok resp)


badHttpBody: String -> HttpError
badHttpBody msg =
  BadBody { url = "", statusCode = 0, statusText = "", headers = Dict.empty } "" msg


httpResult: (Http.Metadata -> String -> Result HttpError a) -> Http.Response String -> Result HttpError a
httpResult decoder response =
  case response of
    Http.BadUrl_ url ->
      Err <| BadUrl url

    Http.Timeout_ ->
      Err <| Timeout

    Http.NetworkError_ ->
      Err <| NetworkError

    Http.BadStatus_ metadata str ->
      Err <| BadStatus metadata str

    Http.GoodStatus_ metadata str ->
      decoder metadata str
