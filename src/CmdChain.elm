module CmdChain exposing
  ( Model, Msg, init, exec, update, isDone
  )


import Task

import Debug exposing (log, toString)


type Model msg model
   = Model (ModelInternal msg model)


type alias ModelInternal msg model =
  { updater: msg -> model -> (model, Cmd msg)
  , modelUpdater: model -> Model msg model -> model
  , cmdChain: List (Cmd msg)
  , done: Bool
  }


type Msg msg
  = ExecMsg (List (Cmd msg))
  | DoNextMsg msg


type alias Tomsg msg = Msg msg -> msg


init: (msg -> model -> (model, Cmd msg)) -> (model -> Model msg model -> model) -> Model msg model
init updater modelUpdater =
  Model <| ModelInternal updater modelUpdater [] False


isDone: Model msg model -> Bool
isDone (Model { done }) = done


exec: Tomsg msg -> List (Cmd msg) -> Cmd msg
exec toMsg cmds =
  Task.perform
    (toMsg << ExecMsg) <|
    Task.succeed <| List.map (Cmd.map (toMsg << DoNextMsg)) cmds


update: Tomsg msg -> Msg msg -> Model msg model -> model -> (model, Cmd msg)
update toMsg msg (Model ({ updater, modelUpdater, cmdChain } as cmodel)) model =
  case msg of
    ExecMsg cmds ->
      ( List.tail cmds |>
        Maybe.map (\rest -> { cmodel | cmdChain = rest}) |>
        Maybe.withDefault cmodel
      , List.head cmds |> Maybe.withDefault Cmd.none
      ) |>
      (\r ->
        Tuple.mapFirst
          (\m -> modelUpdater model <| Model { m | done = Tuple.second r == Cmd.none }
          )
          r
      )

    DoNextMsg nmsg ->
      updater nmsg model |>
      Tuple.mapSecond
        (\cmd ->
          if cmd == Cmd.none then
            Task.perform (toMsg << ExecMsg) <| Task.succeed cmdChain
            else Cmd.map (toMsg << DoNextMsg) cmd
        )
