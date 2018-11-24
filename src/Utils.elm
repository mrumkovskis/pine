module Utils exposing
  ( zip, at, find, set, orElse, httpQuery, matchIdx, strOrEmpty
  , optField, emptyEncoder, noBreakSpace, flip, curry, uncurry, httpErrorToString
  )


{-| Various useful utility methods.

@docs at, httpQuery, matchIdx, noBreakSpace, optField, emptyEncoder
      orElse, strOrEmpty, zip, flip, curry, uncurry, httpErrorToString
-}


import Http
import Dict
import Json.Decode as JD
import Json.Encode as JE
import Url.Builder as UB

import Debug exposing (log, toString)


{-| Zip together to lists. If lists are various size resulting list size
equals to shortest one. This is reverse operation to `List.unzip` function from
core package.
-}
zip: List a -> List b -> List (a, b)
zip list1 list2 =
  let
    z l1 l2 r =
      case (l1, l2) of
        ([], []) -> List.reverse r

        (e1 :: t1, e2 :: t2) -> z t1 t2 <| (e1, e2) :: r

        _ -> r
  in
    z list1 list2 []


{-| Finds list element at specified index.
-}
at: Int -> List a -> Maybe a
at idx l =
  if idx == 0 then List.head l
  else List.tail l |> Maybe.andThen (at (idx - 1))


{-| Find first element index matching condition -}
find: (a -> Bool) -> List a -> Maybe Int
find cond list =
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


{-| Returns first `Just` value `Maybe`
-}
orElse: Maybe.Maybe a -> Maybe.Maybe a -> Maybe.Maybe a
orElse mb2 mb1 = if mb1 == Nothing then mb2 else mb1


{-| Forms http query string from key value list.
-}
httpQuery: List (String, String) -> String
httpQuery params =
  params |>
  List.map (uncurry UB.string) |>
  UB.toQuery


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
httpErrorToString: Http.Error -> String
httpErrorToString err =
  case log "Http error occurred" err of
    Http.BadUrl msg -> msg

    Http.Timeout -> "Timeout occurred"

    Http.NetworkError -> "Network error occurred"

    Http.BadStatus resp -> toString resp

    Http.BadPayload msg resp -> msg ++ "; " ++ toString resp
