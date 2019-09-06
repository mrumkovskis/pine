module Calendar exposing
  ( CalendarEntry, Holiday
  , formatDate, formatDateTime
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
    format "yMd" mask (Dict.fromList [("y", y), ("M", m), ("d", d)])
  ) |>
  Maybe.withDefault value


formatDateTime: String -> String -> String
formatDateTime mask value =
  ymd_hms value |>
  (\((y, m, d), (h, min, s)) ->
    format
      "yMdhms"
      mask
      (Dict.fromList [("y", y), ("M", m), ("d", d), ("h", h), ("m", min), ("s", s)])
  ) |>
  Maybe.withDefault value


format: String -> String -> Dict.Dict String String -> Maybe String
format keys mask comps =
  run (maskParser keys) mask |>
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
                  String.padLeft diff '0' val
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


parse: String -> String -> String
parse mask value =
  ""


dateRegex: Regex
dateRegex =
  r "^(\\d{4})-(\\d{1,2})-(\\d{1,2})$"


dateTimeRegex: Regex
dateTimeRegex =
  r "^(\\d{4})-(\\d{1,2})-(\\d{1,2}).(\\d{1,2}):(\\d{1,2}):(\\d{1,2})$"


maskParser: String -> Parser (List ( Int, String ))
maskParser keys =
  let
    keySet = String.split "" keys |> Set.fromList

    sr = Regex.fromString "[^yMdhms]" |> Maybe.withDefault Regex.never

    sep =
      (getChompedString <| chompWhile (Regex.contains sr << String.fromChar)) |>
      andThen (\res -> if String.isEmpty res then problem "not sep" else succeed res)

    comp c =
      (getChompedString <| chompWhile ((==) c)) |>
      andThen (\res -> if String.isEmpty res then problem "not element" else succeed res)

    compParser =
      succeed Tuple.pair
        |= getOffset
        |= oneOf (sep :: (keys |> String.toList |> List.map comp))

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
