module Calendar exposing
  ( CalendarEntry, Holiday
  , formatDate, formatDateTime
  , parseDate, parseDateTime
  )


import Json.Decode as JD
import Json.Encode as JE
import Dict
import Regex exposing (..)
import Parser exposing (..)
import Set

import JsonModel as JM
import Ask
import Select exposing (..)


import Debug


type alias CalendarEntry =
  { value: Maybe String
  , today: String
  , month: String
  , year: String
  , days: List String
  , weeks: List String
  , holidays: List Holiday
  , date: String
  }


type alias Holiday =
  { date: String
  , name: String
  , description: String
  , is_moved_working_day: Bool
  }


decoder: JD.Decoder (List CalendarEntry)
decoder =
  let
      dec =
        JD.map7
          CalendarEntry
          (JD.field "value" <| JD.maybe JD.string)
          (JD.field "today" JD.string)
          (JD.field "month" JD.string)
          (JD.field "year" JD.string)
          (JD.field "days" <| JD.list JD.string)
          (JD.field "weeks" <| JD.list JD.string)
          (JD.field "holidays" <| JD.list holDec) |>
        JD.andThen
          (\entry_from_date ->
            JD.field "calendar" (JD.list (JD.map entry_from_date JD.string))
          )

      holDec =
        JD.map4
          Holiday
          (JD.field "holiday" JD.string)
          (JD.field "name" JD.string)
          (JD.field "description" JD.string)
          (JD.field "is_moved_working_day" JD.bool)
  in
    JD.oneOf
      [ JD.list dec |>
        JD.andThen (List.head >> Maybe.map JD.succeed >> Maybe.withDefault (JD.fail "invalid data"))
      , dec
      ]


jsonDecoder: JD.Decoder (List JM.JsonValue)
jsonDecoder =
  decoder |>
  JD.map
    ( List.map
        (\entry ->
          [ ("value", entry.value |> Maybe.map JM.JsString |> Maybe.withDefault JM.JsNull)
          , ("today", JM.JsString entry.today)
          , ("month", JM.JsString entry.month)
          , ("year", JM.JsString entry.year)
          , ("date", JM.JsString entry.date)
          , ("days", JM.JsList <| List.map JM.JsString entry.days)
          , ("weeks", JM.JsList <| List.map JM.JsString entry.weeks)
          , ("holidays"
            , JM.JsList <| List.map
                (\h ->
                  [ ("date", JM.JsString h.date)
                  , ("name", JM.JsString h.name)
                  , ("description", JM.JsString h.description)
                  , ("is_moved_working_day", JM.JsBool h.is_moved_working_day)
                  ]
                  |> Dict.fromList |>
                  JM.JsObject
                )
                entry.holidays
            )
          ] |>
          Dict.fromList |>
          JM.JsObject
        )
    )


calendar: Ask.Tomsg msg -> JM.ListModel msg JM.JsonValue
calendar =
  JM.initForm "/metadata" "/data" "/calendar" jsonDecoder (always JE.null) [] (always Nothing)


calendarSelect: String -> Bool -> Tomsg msg JM.JsonValue -> Ask.Tomsg msg -> String -> (String -> msg)
  -> (SelectModel msg JM.JsonValue, Cmd msg)
calendarSelect locale doSearch toMsg toMessagemsg search toDestinationmsg =
  ( Select.init
      (calendar toMessagemsg)
      locale
      search
      (JM.jsonString "value" >> Maybe.withDefault "<vērtība nav atrasta>" >> toDestinationmsg)
      (JM.jsonString "value" >> Maybe.withDefault "<vērtība nav atrasta>")
  , if doSearch then
      Select.search toMsg search
    else
      Cmd.none
  )


formatDate: String -> String -> String
formatDate mask value =
  ymd value |>
  (\(y, m, d) ->
    format mask (Dict.fromList [("y", y), ("M", m), ("d", d)])
  ) |>
  Maybe.withDefault value


formatDateTime: String -> String -> String
formatDateTime mask value =
  ymd_hms value |>
  (\((y, m, d), (h, min, s)) ->
    format
      mask
      (Dict.fromList [("y", y), ("M", m), ("d", d), ("h", h), ("m", min), ("s", s)])
  ) |>
  Maybe.withDefault value


format: String -> Dict.Dict String String -> Maybe String
format mask comps =
  run (maskParser mask) mask |>
  Result.toMaybe |>
  Maybe.map
    ( List.foldl
        (\(_, key) res ->
          let
            getc c =
              Dict.get c comps |>
              Maybe.map adjust |>
              Maybe.withDefault ""

            adjust val =
              let
                  diff = String.length val - String.length key
              in
                if diff > 0 then
                  String.dropLeft diff val
                else
                  String.padLeft (String.length key) '0' val
          in
            res ++
            ( if String.contains "y" key then
                getc "y"
              else if String.contains "M" key then
                getc "M"
              else if String.contains "d" key then
                getc "d"
              else if String.contains "h" key then
                getc "h"
              else if String.contains "m" key then
                getc "m"
              else if String.contains "s" key then
                getc "s"
              else
                key
            )
        )
        ""
    )


parseDate: String -> String -> Maybe String
parseDate =
  parse False


parseDateTime: String -> String -> Maybe String
parseDateTime =
  parse True


parse: Bool -> String -> String -> Maybe String
parse hasTime mask value =
  let
    val = String.trim value
  in
    run (maskParser mask) mask |>
    Result.toMaybe |>
    Maybe.map
      ( List.foldl
          (\(pos, key) (res, off) ->
            let
              comp n l =
                String.slice (pos - off) (pos - off + String.length key) value |>
                String.filter Char.isDigit |>
                (\s ->
                  let d = String.length s - l in
                  ( if d < 0 then String.padLeft l '0' s else String.dropLeft d s
                  , off + String.length key - String.length s
                  )
                ) |>
                (\(v, o) -> (Dict.insert n v res, o))
            in
              if String.contains "y" key then
                comp "y" 4
              else if String.contains "M" key then
                comp "M" 2
              else if String.contains "d" key then
                comp "d" 2
              else if String.contains "h" key then
                comp "h" 2
              else if String.contains "m" key then
                comp "m" 2
              else if String.contains "s" key then
                comp "s" 2
              else
                (res, off)
          )
          (Dict.empty, 0)
      ) |>
    Maybe.map
      (\(res, _) ->
        let
          comp n def =
            Dict.get n res |> Maybe.withDefault def
        in
          String.concat
            [ comp "y" "0000"
            , "-"
            , comp "M" "01"
            , "-"
            , comp "d" "01"
            , " "
            ] ++
          ( if hasTime then
              String.concat
                [ comp "h" "00"
                , ":"
                , comp "m" "00"
                , ":"
                , comp "s" "00"
                , ":"
                ]
            else
              ""
          ) |>
          String.dropRight 1
      )


dateRegex: Regex
dateRegex =
  r "^(\\d{4})-(\\d{1,2})-(\\d{1,2})$"


dateTimeRegex: Regex
dateTimeRegex =
  r "^(\\d{4})-(\\d{1,2})-(\\d{1,2}).(\\d{1,2}):(\\d{1,2}):(\\d{1,2})$"


maskParser: String -> Parser (List ( Int, String ))
maskParser mask =
  let
    ks = Set.fromList <| String.toList "yMdhms"

    sep =
      mask |>
      String.filter (\c -> not <| Set.member c ks) |>
      String.toList |> Set.fromList |> Set.toList |> String.fromList

    keySet = String.split "" (mask |> String.filter (\c -> Set.member c ks)) |> Set.fromList

    sr = Regex.fromString ("[" ++ sep ++ "]") |> Maybe.withDefault Regex.never

    sepParser =
      (getChompedString <| chompWhile (Regex.contains sr << String.fromChar)) |>
      -- check if result is not empty string to ensure that some input is consumed on success to avoid infinite loop
      andThen (\res -> if String.isEmpty res then problem "not sep" else succeed res)

    comp c =
      (getChompedString <| chompWhile ((==) c)) |>
      -- check if result is not empty string to ensure that some input is consumed on success to avoid infinite loop
      andThen (\res -> if String.isEmpty res then problem "not element" else succeed res)

    compParsers =
      keySet |> Set.toList |> String.concat |> String.toList |> List.map comp

    compParser =
      succeed Tuple.pair
        |= getOffset
        |= oneOf (compParsers ++ [ sepParser ])

    validator =
      andThen
        (\comps ->
          comps |>
          List.map (\(_, s) -> String.left 1 s) |>
          List.filter (\s -> Set.member s keySet) |>
          (\res ->
            if List.length res == Set.size (Set.fromList res) then
              succeed comps
            else
              problem "invalid mask, pattern cannot repeat more than once"
          )
        )
  in
    (loop [] <|
      (\comps ->
        oneOf
          [ succeed (\c -> Loop (c :: comps)) |= compParser
          , succeed (Done <| List.reverse comps)
          ]
      )
    ) |>
    validator


ymd: String -> (String, String, String)
ymd date =
  Regex.find dateRegex date |>
  List.head |>
  Maybe.map .submatches |>
  Maybe.andThen
    (\match ->
      case match of
        [ Just y, Just m, Just d ] ->
          Just (y, m, d)

        x ->
          Nothing
    ) |>
  Maybe.withDefault ("<?>", "<?>", "<?>")


ymd_hms: String -> ((String, String, String), (String, String, String))
ymd_hms dt =
  Regex.find dateTimeRegex dt |>
  List.head |>
  Maybe.map .submatches |>
  Maybe.andThen
    (\match ->
      case match of
        [ Just y, Just m, Just d, Just h, Just min, Just s ] ->
          Just ((y, m, d), (h, min, s))

        x ->
          Nothing
    ) |>
  Maybe.withDefault (("<?>", "<?>", "<?>"), ("<?>", "<?>", "<?>"))


r: String -> Regex
r s =
  Regex.fromString s |> Maybe.withDefault Regex.never
