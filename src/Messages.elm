module Messages exposing
  ( Msg (..), Messages
  , init, messages, add, remove, yes, no, clear, update
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

import Dict exposing (Dict)
import Task


{-| Messages storage
-}
type Messages msg =
  Messages (MsgInternal msg)


type alias MsgInternal msg =
  { messages: Dict String (Ask.Msg msg)
  }


{-| Model update messages.
-}
type Msg msg
  = Add (Ask.Msg msg)
  | Remove String
  | Yes String
  | No String
  | Clear


type alias Tomsg msg = (Msg msg -> msg)


{-| Initialize empty message storage.
-}
init: Messages msg
init =
  Messages { messages = Dict.empty }


{-| Gets all messages except unauthorized message
-}
messages: Messages msg -> List (Ask.Msg msg)
messages (Messages m) =
  Dict.values m.messages |>
  List.filter
    (\msg ->
      case msg of
        Ask.Message Ask.Unauthorized _ -> False

        _ -> True
    )


{-| Gets unauthorized message -}
unauthorized: Messages msg -> Maybe (Ask.Msg msg)
unauthorized (Messages m) =
  Dict.values m.messages |>
  List.filter
    (\msg ->
      case msg of
        Ask.Message Ask.Unauthorized _ -> True

        _ -> False
    ) |>
  List.head


{-| Add message.
-}
add: Tomsg msg -> Ask.Msg msg -> Cmd msg
add toMsg msg =
  Task.perform (toMsg << Add) <| Task.succeed msg


{-| Remove message.
-}
remove: Tomsg msg -> String -> Cmd msg
remove toMsg msg =
  msgInternal toMsg Remove msg


{-| Answer yes to [`Question`](Ask#Msg) message.
This triggers associated command execution.
-}
yes: Tomsg msg -> String -> Cmd msg
yes toMsg msg =
  msgInternal toMsg Yes msg


{-| Answer no to [`Question`](Ask#Msg) message.
-}
no: Tomsg msg -> String -> Cmd msg
no toMsg msg =
  msgInternal toMsg No msg


{-| Remove all messages
-}
clear: Tomsg msg -> Cmd msg
clear toMsg =
  Task.perform toMsg <| Task.succeed Clear


msgInternal: Tomsg msg -> (String -> Msg msg) -> String -> Cmd msg
msgInternal toMsg stringTomsg msg =
  Task.perform (toMsg << stringTomsg) <| Task.succeed msg


{-| Model update -}
update: Tomsg msg -> Msg msg -> Messages msg -> (Messages msg, Cmd msg)
update toMsg message (Messages msgs) =
  let
    newModel newMessages = Messages { msgs | messages = newMessages }

    removeMsg msg = newModel <| Dict.remove msg msgs.messages
  in case message of
    Add msg ->
      Tuple.pair
        (newModel <| Dict.insert (Ask.text msg) msg msgs.messages)
        Cmd.none

    Remove msg ->
      Tuple.pair (removeMsg msg) Cmd.none

    Yes msg ->
      msgs.messages |>
      Dict.get msg |>
      Maybe.andThen
        (\m ->
          case m of
            Ask.Question _ cmd ->
              Just <| ( removeMsg msg, cmd ) -- do command associated with Ask message

            _ -> Nothing
        ) |>
      Maybe.withDefault (Tuple.pair (removeMsg msg) Cmd.none)

    No msg ->
      Tuple.pair (removeMsg msg) Cmd.none

    Clear ->
      Tuple.pair (newModel Dict.empty) Cmd.none
