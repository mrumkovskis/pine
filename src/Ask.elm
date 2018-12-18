module Ask exposing
  ( MsgType (..), Msg (..), Tomsg
  , askmsg, ask, info, warn, error, unauthorized
  , errorOrUnauthorized, text
  )


{-| Sends or asks messages.

@docs MsgType, Msg, Tomsg, ask, info, warn, error, text
-}

import Utils

import Task
import Http
import Json.Decode as JD

{-| Message type: Info, Warn, Error
-}
type MsgType
  = Info
  | Warn
  | Error
  | Unauthorized


{-| Command message - generate info, warn on error message - `Message` or
    ask question - `Question`
-}
type Msg msg
  = Message MsgType String
  | Question String (Cmd msg) (Maybe(Cmd msg))


{-| Message constructor -}
type alias Tomsg msg = Msg msg -> msg


askmsg: Tomsg msg -> String -> Cmd msg -> Maybe (Cmd msg) -> msg
askmsg toMsg question cmdYes maybeCmdNo =
  toMsg <| Question question cmdYes maybeCmdNo


{-| Asks `Question`. Question contains command to be performed on positive answer.
-}
ask: Tomsg msg -> String -> Cmd msg -> Maybe (Cmd msg) -> Cmd msg
ask toMsg question cmdYes maybeCmdNo =
  Task.perform toMsg <| Task.succeed <| Question question cmdYes maybeCmdNo


{-| Sends `Info` message
-}
info: Tomsg msg -> String -> Cmd msg
info toMsg message = msgInternal toMsg Info message


{-| Sends `Warn` message
-}
warn: Tomsg msg -> String -> Cmd msg
warn toMsg message = msgInternal toMsg Warn message


{-| Sends `Error` message
-}
error: Tomsg msg -> String -> Cmd msg
error toMsg message = msgInternal toMsg Error message


{-| Sends `Unauthorized` message -}
unauthorized: Tomsg msg -> String -> Cmd msg
unauthorized toMsg message = msgInternal toMsg Unauthorized message


{-| Sends http `Error` or `Unauthorized` on http message -}
errorOrUnauthorized: Tomsg msg -> Http.Error -> Cmd msg
errorOrUnauthorized toMsg err =
  case err of
    Http.BadStatus resp ->
      if resp.status.code == 401 then
        unauthorized
          toMsg
          ( if String.isEmpty resp.status.message then
              "Unauthorized"
            else resp.status.message
          )
      else
        error toMsg <| Utils.httpErrorToString err

    x ->
      error toMsg <| Utils.httpErrorToString x


{-| Retrieves message text.
-}
text: Msg msg -> String
text msg =
  case msg of
    Message _ txt -> txt

    Question txt _ _ -> txt


msgInternal: Tomsg msg -> MsgType -> String -> Cmd msg
msgInternal toMsg msgType message =
  Task.perform toMsg <| Task.succeed <| Message msgType message
