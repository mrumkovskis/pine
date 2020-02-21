module CmdChain exposing
  ( Msg, Tomsg, exec, interrupt, update
  )


import Utils exposing (..)

type alias Updater msg model = msg -> model -> (model, Cmd msg)


type Msg msg
  = ExecMsg (List (Cmd msg)) (Maybe msg)
  | InterruptMsg


type alias Tomsg msg = Msg msg -> msg


type alias Frommsg msg = msg -> Maybe (Msg msg)


exec: Tomsg msg -> List (Cmd msg) -> Cmd msg
exec toMsg cmds =
  do (toMsg << ExecMsg cmds) Nothing


interrupt: Tomsg msg -> Cmd msg
interrupt toMsg =
  do toMsg InterruptMsg


update: Updater msg model -> Frommsg msg -> Tomsg msg -> Msg msg -> model -> (model, Cmd msg)
update updater fromMsg toMsg msg model =
  case msg of
    ExecMsg cmds mmsg ->
      mmsg |>
      Maybe.map
        (\modmsg ->
            fromMsg modmsg |>
            Maybe.map
              (\chmsg ->
                case chmsg of
                  ExecMsg chmds chmmsg ->
                    ( model, do toMsg <| ExecMsg (chmds ++ cmds) chmmsg )

                  InterruptMsg ->
                    ( model, do toMsg InterruptMsg )
              ) |>
            Maybe.withDefault
              ( updater modmsg model |>
                Tuple.mapSecond
                  (\cmd ->
                    if cmd == Cmd.none then
                      do (toMsg << ExecMsg cmds) Nothing
                    else
                      Cmd.map (toMsg << ExecMsg cmds << Just) cmd
                  )
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
