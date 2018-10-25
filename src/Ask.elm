module Ask exposing
  ( MsgType (..), Msg (..), Tomsg
  , ask, info, warn, error, text
  )


{-| Sends or asks messages.
-}


import Task


type MsgType
  = Info
  | Warn
  | Error 


type Msg msg
  = Message MsgType String
  | Question String (Cmd msg)


type alias Tomsg msg = Msg msg -> msg


{-| Asks `Question`. Question contains command to be performed on positive answer.
-}
ask: Tomsg msg -> String -> Cmd msg -> Cmd msg
ask toMsg question cmd =
  Task.perform toMsg <| Task.succeed <| Question question cmd


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


{-| Retrieves message text.
-}
text: Msg msg -> String
text msg =
  case msg of
    Message _ txt -> txt

    Question txt _ -> txt


msgInternal: Tomsg msg -> MsgType -> String -> Cmd msg
msgInternal toMsg msgType message =
  Task.perform toMsg <| Task.succeed <| Message msgType message
