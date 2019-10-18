module Ask exposing
  ( MsgType (..), Msg (..), NavBarAction (..), Tomsg
  , askmsg, ask, info, warn, error, unauthorized
  , askToDeferredmsg, askToCmdChainmsg, pushUrl, replaceUrl, back, forward
  , askToScrollEventsmsg, errorOrUnauthorized, text
  )


{-| Sends or asks messages.

@docs MsgType, Msg, Tomsg, ask, info, warn, error, text
-}

import Utils exposing (..)
import DeferredRequests as DR exposing (Tomsg)
import CmdChain exposing (Tomsg)
import ScrollEvents as SE exposing (Tomsg)

import Task
import Http
import Json.Decode as JD
import Browser.Navigation exposing (Key)

{-| Message type: Info, Warn, Error
-}
type MsgType
  = Info
  | Warn
  | Error
  | Unauthorized


type NavBarAction
  = PushUrl
  | ReplaceUrl
  | Back
  | Forward


{-| Command message - generate info, warn on error message - `Message` or
    ask question - `Question`
-}
type Msg msg
  = Message MsgType String
  | Question String (Cmd msg) (Maybe(Cmd msg))
  | NavBarAction NavBarAction String
  | SubscribeToDeferredMsg (DR.Tomsg msg -> msg)
  | SubscribeToCmdChainMsg (CmdChain.Tomsg msg -> msg)
  | SubscribeToScrollEventsMsg (SE.Tomsg msg -> msg) -- deprecated


{-| Message constructor -}
type alias Tomsg msg = Msg msg -> msg


askmsg: Tomsg msg -> String -> Cmd msg -> Maybe (Cmd msg) -> msg
askmsg toMsg question cmdYes maybeCmdNo =
  toMsg <| Question question cmdYes maybeCmdNo


{-| Asks `Question`. Question contains command to be performed on positive answer.
-}
ask: Tomsg msg -> String -> Cmd msg -> Maybe (Cmd msg) -> Cmd msg
ask toMsg question cmdYes maybeCmdNo =
  do toMsg <| Question question cmdYes maybeCmdNo


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


{-| Ask for `DeferredRequests.Msg` constructor -}
askToDeferredmsg: Tomsg msg -> (DR.Tomsg msg -> msg) -> Cmd msg
askToDeferredmsg toMsg subscription =
  do (toMsg << SubscribeToDeferredMsg) <| subscription


{-| Ask for `CmdChain.Msg` constructor -}
askToCmdChainmsg: Tomsg msg -> (CmdChain.Tomsg msg -> msg) -> Cmd msg
askToCmdChainmsg toMsg subscription =
  do (toMsg << SubscribeToCmdChainMsg) <| subscription


pushUrl: Tomsg msg -> String -> Cmd msg
pushUrl toMsg url =
  do (toMsg << NavBarAction PushUrl) <| url


replaceUrl: Tomsg msg -> String -> Cmd msg
replaceUrl toMsg url =
  do (toMsg << NavBarAction ReplaceUrl) <| url


back: Tomsg msg -> String -> Cmd msg
back toMsg url =
  do (toMsg << NavBarAction Back) <| url


forward: Tomsg msg -> String -> Cmd msg
forward toMsg url =
  do (toMsg << NavBarAction Forward) <| url


askToScrollEventsmsg: Tomsg msg -> (SE.Tomsg msg -> msg) -> Cmd msg
askToScrollEventsmsg toMsg subscription =
  do (toMsg << SubscribeToScrollEventsMsg) <| subscription


{-| Sends http `Error` or `Unauthorized` on http message -}
errorOrUnauthorized: Tomsg msg -> HttpError -> Cmd msg
errorOrUnauthorized toMsg err =
  case err of
    BadStatus { statusCode, statusText } _ ->
      if statusCode == 401 then
        unauthorized toMsg (if String.isEmpty statusText then "Unauthorized" else statusText)
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

    _ -> ""

msgInternal: Tomsg msg -> MsgType -> String -> Cmd msg
msgInternal toMsg msgType message =
  do toMsg <| Message msgType message
