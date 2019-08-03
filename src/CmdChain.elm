module CmdChain exposing
  ( Msg, Tomsg, exec, update
  )


import Utils exposing (..)

import Task

import Debug exposing (log, toString)


type alias Updater msg model = msg -> model -> (model, Cmd msg)


type Msg msg
  = ExecMsg (List (Cmd msg))
  | DoNextMsg (List (Cmd msg)) msg


type alias Tomsg msg = Msg msg -> msg


exec: Tomsg msg -> List (Cmd msg) -> Cmd msg
exec toMsg cmds =
  do (toMsg << ExecMsg) <| cmds


update: Updater msg model -> Tomsg msg -> Msg msg -> model -> (model, Cmd msg)
update updater toMsg msg model =
  case msg of
    ExecMsg cmds ->
      case cmds of
        [] ->
          ( model, Cmd.none )

        cmd :: rest ->
          ( model, Cmd.map (toMsg << DoNextMsg rest) cmd)

    DoNextMsg cmds nmsg ->
      updater nmsg model |>
      Tuple.mapSecond
        (\cmd ->
          if cmd == Cmd.none then
            do (toMsg << ExecMsg) cmds
          else
            Cmd.map (toMsg << DoNextMsg cmds) cmd
        )
