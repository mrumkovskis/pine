module Calendar exposing
  ( CalendarEntry, Holiday
  )


import Json.Decode as JD
import Json.Encode as JE
import Dict
import Regex exposing (..)

import JsonModel as JM
import Ask
import Select exposing (..)


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


format: String -> String -> String
format pattern date =
  ""

parse: String -> String -> String
parse pattern date =
  ""


dateRegex: Regex
dateRegex =
  Regex.fromString "^(\\d{4})-(\\d{2})-(\\d{2})$" |> Maybe.withDefault Regex.never


dateTimeRegex: Regex
dateTimeRegex =
  Regex.fromString "^(\\d{4})-(\\d{2})-(\\d{2}).(\\d{2}):(\\d{2}):(\\d{2})$" |>
  Maybe.withDefault Regex.never


separatorRegex: Regex
separatorRegex =
  Regex.fromString "[^\\w]" |> Maybe.withDefault Regex.never


separators: String -> List (Int, String)
separators pattern =
  components separatorRegex pattern


dateMaskRegex: Regex
dateMaskRegex =
  Regex.fromString "(y{2,4}|M{2}|d{2}){1,3}" |> Maybe.withDefault Regex.never


dateTimeMaskRegex: Regex
dateTimeMaskRegex =
  Regex.fromString "(y{2,4}|M{2}|d{2}|h{2}|m{2}|s{2}){1,6}" |> Maybe.withDefault Regex.never


dateComponents: String -> List(Int, String)
dateComponents pattern =
  components dateMaskRegex pattern


components: Regex -> String -> List (Int, String)
components regex pattern =
  Regex.find regex pattern |>
  List.map (\m -> (m.index, m.match))
