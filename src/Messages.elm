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
-}


import Ask

import Dict exposing (Dict)
import Task


type Messages msg =
  Messages (MsgInternal msg)


type alias MsgInternal msg =
  { messages: Dict String (Ask.Msg msg)
  }


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


{-| Gets all messages.
-}
messages: Messages msg -> List (Ask.Msg msg)
messages (Messages { messages }) =
  Dict.values messages


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


update: Tomsg msg -> Msg msg -> Messages msg -> (Messages msg, Cmd msg)
update toMsg message (Messages messages) =
  let
    newModel newMessages = Messages { messages | messages = newMessages }

    remove msg = newModel <| Dict.remove msg messages.messages
  in case message of
    Add msg ->
      (newModel <| Dict.insert (Ask.text msg) msg messages.messages)
        ! []

    Remove msg ->
      (remove msg)
        ! []

    Yes msg ->
      messages.messages |>
      Dict.get msg |>
      Maybe.andThen
        (\message ->
          case message of
            Ask.Question _ cmd ->
              Just <| remove msg ! [ cmd ] -- do command associated with Ask message

            _ -> Nothing
        ) |>
      Maybe.withDefault ((remove msg) ! [])

    No msg ->
      (remove msg)
        ! []

    Clear ->
      (newModel Dict.empty)
        ! []
