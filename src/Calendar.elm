module Calendar exposing
  ( CalendarEntry, Holiday
  )


import Json.Decode as JD
import Json.Encode as JE
import Dict

import JsonModel as JM
import Ask
import Select exposing (..)


type alias CalendarEntry =
  { value: String
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
          (JD.field "value" JD.string)
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
          [ ("value", JM.JsString entry.value)
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
