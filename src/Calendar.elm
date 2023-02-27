module Calendar exposing
  ( Calendar, Holiday
  , dateController, dateTimeController
  , dateValidator, dateTimeValidator, dateValidatorTask, dateTimeValidatorTask
  , dateUpdaterTask, dateTimeUpdaterTask
  , dateInputParser, dateTimeInputParser
  , formatDate, formatDateTime
  , parseDate, parseDateTime
  )


import Json.Decode as JD
import Json.Encode as JE
import Dict exposing (..)
import Regex exposing (..)
import Parser exposing (..)
import Set
import Task

import JsonModel as JM
import EditModel as EM
import ViewMetadata as VM
import Select exposing (..)
import Utils exposing (required)


type alias Calendar =
  { value: Maybe String
  , today: String
  , month: String
  , month_int: Int
  , year: String
  , days: List String
  , weeks: List String
  , holidays: List Holiday
  , calendar: List String
  }


type alias Holiday =
  { date: String
  , name: String
  , description: String
  , is_moved_working_day: Bool
  }


decoder: JD.Decoder (List Calendar)
decoder =
  let
      calendarDecoder =
        JD.succeed Calendar
          |> required "value" (JD.string |> JD.maybe)
          |> required "today" JD.string
          |> required "month" JD.string
          |> required "month_int" (JD.map (\float -> truncate float) JD.float)
          |> required "year" JD.string
          |> required "days" (JD.list JD.string)
          |> required "weeks" (JD.list JD.string)
          |> required "holidays" (JD.list holidayDecoder)
          |> required "calendar" (JD.list JD.string)

      holidayDecoder =
        JD.map4
          Holiday
          (JD.field "holiday" JD.string)
          (JD.field "name" JD.string)
          (JD.field "description" Utils.strOrEmpty)
          (JD.field "is_moved_working_day" JD.bool)
  in
    JD.list calendarDecoder


jsonDecoder: JD.Decoder (List JM.JsonValue)
jsonDecoder =
  decoder |>
  JD.map
    ( List.map
        (\entry ->
          [ ("value", JM.JsString <| Maybe.withDefault "" <| entry.value)
          , ("today", JM.JsString entry.today)
          , ("month", JM.JsString entry.month)
          , ("month_int", JM.JsNumber (toFloat entry.month_int))
          , ("year", JM.JsString entry.year)
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
          , ("calendar", JM.JsList <| List.map JM.JsString entry.calendar)
          ] |>
          Dict.fromList |>
          JM.JsObject
        )
    )


calendarSelect: String -> String -> Bool -> EM.SelectInitializer msg
calendarSelect locale mask doSearch toMsg toMessagemsg { value } toDestinationmsg _ =
  let
    calendar =
      JM.initForm "/metadata" "/data" "calendar" jsonDecoder (always JE.null) [] (always Nothing)
  in
    ( Select.init
        (calendar toMessagemsg)
        locale
        value
        ( JM.jsonString "value" >>
          Maybe.map (formatDate mask) >>
          Maybe.withDefault "<vērtība nav atrasta>" >>
          toDestinationmsg
        )
        ( JM.jsonString "value" >>
          Maybe.map (formatDate mask) >>
          Maybe.withDefault "<vērtība nav atrasta>"
        )
    , if doSearch then
        Select.search toMsg value
      else
        Cmd.none
    )


timeSelect: String -> String -> Bool -> EM.SelectInitializer msg
timeSelect locale mask doSearch toMsg toMessagemsg { value } toDestinationmsg _ =
  let
    time =
      JM.initForm "/metadata" "/data" "calendar_time" jsonDecoder (always JE.null) [] (always Nothing)

  in
    ( Select.init
        (time toMessagemsg)
        locale
        value
        ( JM.jsonString "value" >>
          Maybe.map (formatDateTime mask) >>
          Maybe.withDefault "<vērtība nav atrasta>" >>
          toDestinationmsg
        )
        ( JM.jsonString "value" >>
          Maybe.map (formatDateTime mask) >>
          Maybe.withDefault "<vērtība nav atrasta>"
        )
    , if doSearch then
        Select.search toMsg value
      else
        Cmd.none
    )


dateValidatorTask: Dict Char Char -> String -> String -> EM.InputValidator msg
dateValidatorTask =
  validatorTask "calendar"


dateTimeValidatorTask: Dict Char Char -> String -> String -> EM.InputValidator msg
dateTimeValidatorTask =
  validatorTask "calendar_time"


validatorTask: String -> Dict Char Char -> String -> String -> EM.InputValidator msg
validatorTask url map locale mask { name, value } _ =
  Utils.httpGetJson ("/data/" ++ url ++ Utils.httpQuery [(locale, value)]) decoder |>
  Task.mapError Utils.httpErrorToString |>
  Task.map
    (\cal ->
      if List.isEmpty cal && (not <| String.isEmpty value) then
        [( name, localizedMask map mask)]
      else
        [( name, "")]
    ) |>
  EM.ValidationTask


dateUpdaterTask: String -> EM.ModelUpdater msg JM.JsonValue
dateUpdaterTask =
  updateTask "calendar"


dateTimeUpdaterTask: String -> EM.ModelUpdater msg JM.JsonValue
dateTimeUpdaterTask =
  updateTask "calendar_time"


updateTask: String -> String -> EM.ModelUpdater msg JM.JsonValue
updateTask url locale input _ =
  Utils.httpGetJson ("/data/" ++ url ++ Utils.httpQuery [(locale, input.value)]) decoder |>
  Task.mapError Utils.httpErrorToString |>
  Task.map
    (\cal ->
      ( List.head cal |> Maybe.andThen .value |> Maybe.withDefault input.value |> JM.JsString
      , JM.jsonEdit input.name
      )
    ) |>
  EM.UpdateTask


dateValidator: Dict Char Char -> String -> EM.InputValidator msg
dateValidator map mask =
  \{ name, value } _ ->
    let
      valres msg =
        [ ( name, msg ) ]
    in
      parseDate mask value |>
      Maybe.map (\_ -> valres "") |>
      Maybe.withDefault (valres <| localizedMask map mask) |>
      EM.ValidationResult


dateTimeValidator: Dict Char Char -> String -> EM.InputValidator msg
dateTimeValidator map mask =
  \{ name, value } _ ->
    let
      valres msg =
        [ ( name, msg ) ]
    in
      parseDateTime mask value |>
      Maybe.map (\_ -> valres "") |>
      Maybe.withDefault (valres <| localizedMask map mask) |>
      EM.ValidationResult



dateFormatter: String -> JM.JsonValue -> String
dateFormatter mask =
  JM.jsonValueToString >> formatDate mask


dateInputParser: String -> EM.Input msg -> EM.Input msg
dateInputParser mask inp =
  { inp | value = parseDate mask inp.value |> Maybe.withDefault inp.value }


dateTimeFormatter: String -> JM.JsonValue -> String
dateTimeFormatter mask =
  JM.jsonValueToString >> formatDateTime mask


dateTimeInputParser: String -> EM.Input msg -> EM.Input msg
dateTimeInputParser mask inp =
  { inp | value = parseDateTime mask inp.value |> Maybe.withDefault inp.value }


localizedMask: Dict Char Char -> String -> String
localizedMask map =
  String.map (\c -> Dict.get c map |> Maybe.withDefault c)


dateController: String -> String -> Dict Char Char -> String -> Bool -> JM.Path -> VM.Field -> EM.Controller msg
dateController dataBaseUrl locale map mask doSearch path field =
  EM.defaultController dataBaseUrl path field |>
  EM.withFormatter (dateFormatter mask) |>
  EM.withValidator (dateValidatorTask map locale mask) |>
  EM.withUpdater (dateUpdaterTask locale) |>
  --EM.withParser (dateInputParser mask) |>
  --EM.withValidator (dateValidator map mask) |>
  EM.withSelectInitializer (calendarSelect locale mask doSearch)


dateTimeController: String -> String -> Dict Char Char -> String -> Bool -> JM.Path -> VM.Field -> EM.Controller msg
dateTimeController dataBaseUrl locale map mask doSearch path field =
  EM.defaultController dataBaseUrl path field |>
  EM.withFormatter (dateTimeFormatter mask) |>
  EM.withValidator (dateTimeValidatorTask map locale mask) |>
  EM.withUpdater (dateTimeUpdaterTask locale) |>
  --EM.withParser (dateTimeInputParser mask) |>
  --EM.withValidator (dateTimeValidator map mask) |>
  EM.withSelectInitializer (timeSelect locale mask doSearch)


formatDate: String -> String -> String
formatDate mask value =
  ymd value |>
  Maybe.andThen
    (\(y, m, d) ->
      format mask (Dict.fromList [("y", y), ("M", m), ("d", d)])
    ) |>
  Maybe.withDefault value


formatDateTime: String -> String -> String
formatDateTime mask value =
  ymd_hms value |>
  Maybe.andThen
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
    if String.isEmpty val then
      Just ""
    else
      run (maskParser mask) mask |>
      Result.toMaybe |>
      Maybe.map
        ( List.foldl
            (\(pos, key) (res, off) ->
              let
                comp n l def =
                  String.slice (pos - off) (pos - off + String.length key) val |>
                  String.filter Char.isDigit |>
                  (\s ->
                    if String.isEmpty s then
                      ( def
                      , off + String.length key - String.length def
                      )
                    else
                      let d = String.length s - l in
                      ( if d < 0 then String.padLeft l '0' s else String.dropLeft d s
                      , off + String.length key - String.length s
                      )
                  ) |>
                  (\(v, o) -> (Dict.insert n v res, o))
              in
                if String.contains "y" key then
                  comp "y" 4 ""
                else if String.contains "M" key then
                  comp "M" 2 ""
                else if String.contains "d" key then
                  comp "d" 2 ""
                else if String.contains "h" key then
                  comp "h" 2 ""
                else if String.contains "m" key then
                  comp "m" 2 ""
                else if String.contains "s" key then
                  comp "s" 2 ""
                else
                  (res, off)
            )
            (Dict.empty, 0)
        ) |>
      Maybe.andThen
        (\(d, _) -> -- validate if some components are set
          if Dict.size d == (Dict.size <| Dict.filter (always <| (==) "") d) then
            Nothing
          else Just d
        ) |>
      Maybe.map
        (\res ->
          let
            comp n def =
              Dict.get n res |> Maybe.withDefault def
          in
            String.concat
              [ comp "y" "2000"
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


ymd: String -> Maybe (String, String, String)
ymd date =
  Regex.find dateRegex date |>
  List.head |>
  Maybe.map .submatches |>
  Maybe.andThen
    (\match ->
      case match of
        [ Just y, Just m, Just d ] ->
          Just (y, m, d)

        _ ->
          Nothing
    )


ymd_hms: String -> Maybe ((String, String, String), (String, String, String))
ymd_hms dt =
  Regex.find dateTimeRegex dt |>
  List.head |>
  Maybe.map .submatches |>
  Maybe.andThen
    (\match ->
      case match of
        [ Just y, Just m, Just d, Just h, Just min, Just s ] ->
          Just ((y, m, d), (h, min, s))

        _ ->
          Nothing
    )


r: String -> Regex
r s =
  Regex.fromString s |> Maybe.withDefault Regex.never
