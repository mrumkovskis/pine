module Messages exposing
  ( Msg (..), Messages
  , init, messages, unauthorized
  , add, remove, yes, no, clear, flags, update
  )


{-| Stores [`Ask`](Ask#Msg) messages.

# Initialization, configuration
@docs init

# Commands
@docs add, remove, yes, no, clear

# Data examination
@docs messages

# Types & update
@docs Messages, Msg, update
-}


import Ask
import Utils exposing (..)

import Dict exposing (Dict)


{-| Messages storage
-}
type Messages msg flags =
  Messages (MsgInternal msg flags)


type alias MsgInternal msg flags =
  { messages: Dict String (Ask.Msg msg, flags)
  }


{-| Model update messages.
-}
type Msg msg flags
  = Add (Ask.Msg msg) flags
  | Remove String
  | Yes String
  | No String
  | Flags String flags
  | Clear


type alias Tomsg msg flags = (Msg msg flags -> msg)


{-| Initialize empty message storage.
-}
init: Messages msg flags
init =
  Messages { messages = Dict.empty }


{-| Gets all messages except unauthorized message
-}
messages: Messages msg flags -> List (Ask.Msg msg, flags)
messages (Messages m) =
  Dict.values m.messages |>
  List.filter
    (\(msg, _) ->
      case msg of
        Ask.Message Ask.Unauthorized _ -> False

        _ -> True
    )


{-| Gets unauthorized message -}
unauthorized: Messages msg flags -> Maybe (Ask.Msg msg, flags)
unauthorized (Messages m) =
  Dict.values m.messages |>
  List.filter
    (\(msg, _) ->
      case msg of
        Ask.Message Ask.Unauthorized _ -> True

        _ -> False
    ) |>
  List.head


{-| Add message.
-}
add: Tomsg msg flags -> Ask.Msg msg -> flags -> Cmd msg
add toMsg msg msgflags =
  do (toMsg << Add msg) <| msgflags


{-| Remove message.
-}
remove: Tomsg msg flags -> String -> Cmd msg
remove toMsg msg =
  msgInternal toMsg Remove msg


{-| Answer yes to [`Question`](Ask#Msg) message.
This triggers associated command execution.
-}
yes: Tomsg msg flags -> String -> Cmd msg
yes toMsg msg =
  msgInternal toMsg Yes msg


{-| Answer no to [`Question`](Ask#Msg) message.
-}
no: Tomsg msg flags -> String -> Cmd msg
no toMsg msg =
  msgInternal toMsg No msg


{-| Remove all messages
-}
clear: Tomsg msg flags -> Cmd msg
clear toMsg =
  do toMsg <| Clear


{-| Updates messages flags -}
flags: Tomsg msg flags -> String -> flags -> Cmd msg
flags toMsg msg msgflags =
  do (toMsg << Flags msg) <| msgflags


msgInternal: Tomsg msg flags -> (String -> Msg msg flags) -> String -> Cmd msg
msgInternal toMsg stringTomsg msg =
  do (toMsg << stringTomsg) <| msg


{-| Model update -}
update: Tomsg msg flags -> Msg msg flags -> Messages msg flags -> (Messages msg flags, Cmd msg)
update _ message (Messages msgs) =
  let
    newModel newMessages = Messages { msgs | messages = newMessages }

    removeMsg msg = newModel <| Dict.remove msg msgs.messages
  in case message of
    Add msg msgflags ->
      Tuple.pair
        (newModel <| Dict.insert (Ask.text msg) (msg, msgflags) msgs.messages)
        Cmd.none

    Remove msg ->
      Tuple.pair (removeMsg msg) Cmd.none

    Yes msg ->
      msgs.messages |>
      Dict.get msg |>
      Maybe.andThen
        (\(m, _) ->
          case m of
            Ask.Question _ cmd _ ->
              Just <| ( removeMsg msg, cmd ) -- do yes command associated with Ask message

            _ -> Nothing
        ) |>
      Maybe.withDefault (Tuple.pair (removeMsg msg) Cmd.none)

    No msg ->
      msgs.messages |>
      Dict.get msg |>
      Maybe.andThen
        (\(m, _) ->
          case m of
            Ask.Question _ _ maybeCmd ->
              Just <| ( removeMsg msg, maybeCmd |> Maybe.withDefault Cmd.none ) -- do no command associated with Ask message

            _ -> Nothing
        ) |>
      Maybe.withDefault (Tuple.pair (removeMsg msg) Cmd.none)

    Flags msg msgflags ->
      ( newModel <| Dict.update msg (Maybe.map (\(m, _) -> (m, msgflags))) msgs.messages
      , Cmd.none
      )

    Clear ->
      Tuple.pair (newModel Dict.empty) Cmd.none
