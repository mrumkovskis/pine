module CmdChain exposing
  ( Msg (..), Tomsg, exec, update
  )


import Utils exposing (..)

type alias Updater msg model = msg -> model -> (model, Cmd msg)


type Msg msg
  = ExecMsg (List (Cmd msg)) (Maybe msg)
  | InterruptMsg


type alias Tomsg msg = Msg msg -> msg


exec: Tomsg msg -> List (Cmd msg) -> Cmd msg
exec toMsg cmds =
  do (toMsg << ExecMsg cmds) Nothing


update: Updater msg model -> Tomsg msg -> Msg msg -> model -> (model, Cmd msg)
update updater toMsg msg model =
  case msg of
    ExecMsg cmds mmsg ->
      mmsg |>
      Maybe.map
        (\modmsg ->
            updater modmsg model |>
            Tuple.mapSecond
              (\cmd ->
                if cmd == Cmd.none then
                  do (toMsg << ExecMsg cmds) Nothing
                else
                  Cmd.map (toMsg << ExecMsg cmds << Just) cmd
              )

        ) |>
      Maybe.withDefault
        ( model
        , case cmds of
            [] ->
              Cmd.none

            cmd :: rest ->
              Cmd.map (toMsg << ExecMsg rest << Just) cmd
        )

    InterruptMsg ->
      ( model, Cmd.none )
