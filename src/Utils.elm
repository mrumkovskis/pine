module Utils exposing
  ( zip
  , at
  , orElse
  , httpQuery
  , matchIdx
  , strOrEmpty
  , optField
  , noBreakSpace
  )

import Http
import Dict
import Json.Decode as JD

zip: List a -> List b -> List (a, b)
zip l1 l2 =
  let
    zip l1 l2 r =
      case (l1, l2) of
        ([], []) -> List.reverse r

        (e1 :: t1, e2 :: t2) -> zip t1 t2 <| (e1, e2) :: r

        _ -> r
  in
    zip l1 l2 []


at: Int -> List a -> Maybe a
at idx l =
  if idx == 0 then List.head l
  else List.tail l |> Maybe.andThen (at (idx - 1))


orElse: Maybe.Maybe a -> Maybe.Maybe a -> Maybe.Maybe a
orElse mb2 mb1 = if mb1 == Nothing then mb2 else mb1


httpQuery: List (String, String) -> String
httpQuery params =
  String.join "&" <| List.map (\(k,v) -> Http.encodeUri k ++ "=" ++ Http.encodeUri v) params


noBreakSpace: String
noBreakSpace = String.fromChar '\x00A0'


{- Finds index of element most closely matching pattern.
   Matching priority:
     1. Exact match
     2. Exact case insensitive without latvian diacritic match
     3. Starts with match
     4. Starts with case insensitive without latvian diacritic match
-}
matchIdx: String -> List String -> Maybe Int
matchIdx pattern list =
  let
    stringMatch pattern string =
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
    List.indexedMap (,) |>
    List.map (\(i, s) -> (i, stringMatch pattern s)) |>
    List.filter (\(_, (r, _)) -> not <| r == 5) |>
    List.sortBy (\(_, r) -> r) |>
    List.head |>
    Maybe.map Tuple.first


strOrEmpty: JD.Decoder String
strOrEmpty =
  JD.oneOf [ JD.string, JD.null "" ]


optField: String -> JD.Decoder a -> JD.Decoder (Maybe a)
optField name decoder =
  JD.maybe (JD.field name decoder)
