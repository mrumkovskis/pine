module FormModel exposing
  ( Model, Msg, Tomsg
  , init, toModelMsg
  , createMsg, saveMsg, editMsg, cancelMsg, deleteMsg
  , create, save, edit, cancel, delete
  , update
  )


import EditModel as EM
import JsonModel as JM
import Ask
import Utils exposing (..)

import Task

type alias Model msg =
  { init: () -> EM.JsonEditModel msg
  , form: Maybe (EM.JsonEditModel msg)
  , toMessagemsg: Ask.Tomsg msg
  }


type Msg msg
  = CreateMsg (EM.JsonEditMsg msg)
  | EditMsg JM.JsonValue
  | CancelEditMsg Bool
  | SaveMsg (Maybe msg) (EM.JsonEditMsg msg)
  | DeleteMsg (Maybe msg) Int


type alias Tomsg msg = Msg msg -> msg


init: (() -> EM.JsonEditModel msg) -> Ask.Tomsg msg -> Model msg
init initializer toMessagemsg =
  Model initializer Nothing toMessagemsg


toModelMsg: Tomsg msg -> (EM.JsonEditMsg msg -> msg)
toModelMsg toMsg =
  toMsg << CreateMsg


createMsg: Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> msg
createMsg toMsg searchParams createFun =
  EM.createMsg (toModelMsg toMsg) searchParams createFun


create: Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> Cmd msg
create toMsg searchParams createFun =
  domsg <| createMsg toMsg searchParams createFun


saveMsg: Tomsg msg -> Maybe msg -> msg
saveMsg toMsg maybeSuccessmsg =
  EM.saveMsg (toMsg << SaveMsg maybeSuccessmsg)


save: Tomsg msg -> Maybe msg -> Cmd msg
save toMsg maybeSuccessmsg =
  domsg <| saveMsg toMsg maybeSuccessmsg


editMsg: Tomsg msg -> JM.JsonValue -> msg
editMsg toMsg data =
  toMsg <| EditMsg data


edit: Tomsg msg -> JM.JsonValue -> Cmd msg
edit toMsg data =
  domsg <| editMsg toMsg data


cancelMsg: Tomsg msg -> Bool -> msg
cancelMsg toMsg ask =
  toMsg <| CancelEditMsg ask


cancel: Tomsg msg -> Bool -> Cmd msg
cancel toMsg ask =
  domsg <| cancelMsg toMsg ask


deleteMsg: Tomsg msg -> Maybe msg -> Int -> msg
deleteMsg toMsg maybeSuccessmsg id =
  toMsg <| DeleteMsg maybeSuccessmsg id


delete: Tomsg msg -> Maybe msg -> Int -> Cmd msg
delete toMsg maybeSuccessmsg id =
    domsg <| deleteMsg toMsg maybeSuccessmsg id


update: Tomsg msg -> Msg msg -> Model msg -> (Model msg, Cmd msg)
update toMsg msg ({ form, toMessagemsg } as model) =
  case msg of
    CreateMsg data ->
      form |>
      Utils.orElse (Just <| model.init ()) |>
      Maybe.map (EM.update (toMsg << CreateMsg) data) |>
      Maybe.map
        (Tuple.mapFirst (\m -> { model | form = Just m})) |>
      Maybe.withDefault ( model, Cmd.none )

    EditMsg data ->
      ( model
      , JM.jsonInt "id" data |>
        Maybe.map (EM.fetch (toMsg << CreateMsg)) |>
        Maybe.withDefault Cmd.none
      )

    CancelEditMsg ask ->
      if ask && model.form /= Nothing then
        ( model
        , Ask.ask
            toMessagemsg
            "Vai atcelt datu labošanu?"
            (cancel toMsg False)
            Nothing
        )
      else
        ( { model | form = Nothing }, Cmd.none )

    SaveMsg maybeSuccessmsg data ->
      form |>
      Maybe.map (EM.update (toMsg << SaveMsg maybeSuccessmsg) data) |>
      Maybe.map (Tuple.mapFirst (\m -> { model | form = Just m})) |>
      Maybe.withDefault ( model, Cmd.none ) |>
      (\(newmod, cmd) ->
        if cmd == Cmd.none then
          ( { model | form = Nothing }
          , maybeSuccessmsg |>
            Maybe.map Task.succeed |>
            Maybe.map (Task.perform identity) |>
            Maybe.withDefault Cmd.none
          )
        else ( newmod, cmd )
      )

    DeleteMsg maybeSuccessmsg id ->
      Tuple.pair
        model <|
        Ask.ask
          toMessagemsg
          "Vai dzēst ierakstu?"
          (EM.delete (toMsg << SaveMsg maybeSuccessmsg) id)
          Nothing
