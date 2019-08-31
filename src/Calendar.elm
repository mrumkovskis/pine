module Calendar exposing
  ( CalendarEntry, Holiday
  )


import Json.Decode as JD
import Json.Encode as JE

import JsonModel as JM
import Ask


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


calendar: Ask.Tomsg msg -> JM.ListModel msg CalendarEntry
calendar =
  JM.initForm "/metadata" "/data" "/calendar" decoder (always JE.null) [] (always Nothing)
