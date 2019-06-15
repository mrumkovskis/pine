module FormModel exposing
  ( Model, Msg, Tomsg
  , init, toModelMsg, create, save, edit, cancel, delete
  , update
  )


import EditModel as EM
import JsonModel as JM
import Ask
import Utils

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


create: Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> msg
create toMsg searchParams createFun =
  EM.createMsg (toModelMsg toMsg) searchParams createFun


save: Tomsg msg -> Maybe msg -> msg
save toMsg maybeSuccessmsg =
  EM.saveMsg (toMsg << SaveMsg maybeSuccessmsg)


edit: Tomsg msg -> JM.JsonValue -> msg
edit toMsg data =
  toMsg <| EditMsg data


cancel: Tomsg msg -> Bool -> msg
cancel toMsg ask =
  toMsg <| CancelEditMsg ask


delete: Tomsg msg -> Maybe msg -> Int -> msg
delete toMsg maybeSuccessmsg id =
  toMsg <| DeleteMsg maybeSuccessmsg id


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
      if ask then
        ( model
        , Ask.ask
            toMessagemsg
            "Vai atcelt datu labošanu?"
            (Task.perform identity <| Task.succeed <| cancel toMsg False)
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
