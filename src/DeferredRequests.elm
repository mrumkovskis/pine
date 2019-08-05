module DeferredRequests exposing
  ( DeferredRequest, DeferredStatus (..), Tomsg, Model, Msg, Subscription, Config
  , init, decoder, update, subscribeCmd, askCmd, subscribeOrAskCmd, onHttpErrorCmd
  , wsSubscriptions, requests, subscriptions
  )


{-| Deferred request notification subscription module.

# Initialization, configuration
@docs init

# Commands
@docs subscribeCmd, maybeSubscribeCmd

# Data examination
@docs requests, subscriptions

# Types & other
@docs DeferredRequest, DeferredStatus, Model, Msg, Tomsg,
      update, wsSubscriptions
-}


import Utils exposing (..)

import Json.Decode as JD
import Dict exposing (Dict)
import Http
-- import WebSocket
import Task
import Debug exposing (toString)


{-| Deferred request structure, consists of status and notification time.
-}
type alias DeferredRequest =
  { status: DeferredStatus
  , time: String
  }


{-| Deferred status
-}
type DeferredStatus
  = OK
  | ERR
  | EXE


type alias DeferredHeader = (String, String)


{-| Msg constructor type alias
-}
type alias Tomsg msg = Msg msg -> msg


{-| Subscription to deferred result.
-}
type alias Subscription msg = (Result HttpError JD.Value) -> msg


type alias DeferredQuestion msg = String -> DeferredHeader -> msg


type alias Config =
  { deferredResultBaseUri: String
  , wsNotificationUri: String
  , deferredHeader: DeferredHeader
  , isTimeoutErr: String -> Bool
  , timeoutQuestion: String
  , decoder: JD.Decoder String
  }


{-| Model
-}
type Model msg = Model (Dict String DeferredRequest) (Dict String (Subscription msg)) Config


{-| Messages. Sent to update deferred request statuses and subscribe to deferred results.
-}
type Msg msg
  = UpdateMsg String
  | SubscribeMsg String (Subscription msg)
  | MaybeSubscribeMsg String (Subscription msg) (Bool -> msg)
  | MaybeAskMsg String (DeferredQuestion msg) (Bool -> msg)
  | MaybeSubscribeOrAskMsg String (Subscription msg) (DeferredQuestion msg) (Bool -> msg)


{-| Creates model. First argument specifies base uri where to get ready results
(real uri appends requests id to the base), second argument is web socket notification
uri of request execution progress. Third is deferred http header name like `x-deferred`.
Fourth is defaultTimeout like `180s`. Fifth - question to display for user to ask whether redo request with
deferred header.
-}
init: Config -> Model msg
init config =
  Model Dict.empty Dict.empty config


decoder: JD.Decoder String
decoder = JD.field "deferred" JD.string


{-| Get active requests. Key is request id.
-}
requests: Model msg -> Dict String DeferredRequest
requests (Model r _ _) = r


{-| Get active subscriptions. List element is request id.
-}
subscriptions: Model msg -> List String
subscriptions (Model _ s _) = Dict.keys s


{-| Maybe subscribe to deferred result. Subscription is successful if
third argument `deferredResponse` corresponds following json pattern:

    `{"deferred":"w4OIXzaWt2437gZ1fqxMsFAHXVA"}`
-}
subscribeCmd: Tomsg msg -> Subscription msg -> String -> (Bool -> msg) -> Cmd msg
subscribeCmd toMsg subscription deferredResponse toResmsg =
  do toMsg <|
    MaybeSubscribeMsg deferredResponse subscription toResmsg


askCmd: Tomsg msg -> String -> DeferredQuestion msg -> (Bool -> msg) -> Cmd msg
askCmd toMsg deferredResponse toAskmsg toResmsg =
  do toMsg <|
    MaybeAskMsg deferredResponse toAskmsg toResmsg


subscribeOrAskCmd: Tomsg msg -> Subscription msg -> String -> DeferredQuestion msg -> (Bool -> msg) -> Cmd msg
subscribeOrAskCmd toMsg subscription deferredResponse toAskmsg toResmsg =
  do toMsg <|
    MaybeSubscribeOrAskMsg deferredResponse subscription toAskmsg toResmsg


onHttpErrorCmd: Tomsg msg -> Subscription msg -> HttpError -> DeferredQuestion msg -> (Bool -> msg) -> Cmd msg
onHttpErrorCmd toMsg subscription error toAskmsg toResmsg =
  case error of
    BadBody _ response _ ->
      subscribeOrAskCmd toMsg subscription response toAskmsg toResmsg

    BadStatus _ response ->
      askCmd toMsg response toAskmsg toResmsg

    err ->
      do toResmsg <| False


{-| Model update.
-}
update: Tomsg msg -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update toMsg msg (Model reqs subs conf as model) =
  let
    same = Model reqs subs conf

    notificationsDecoder =
      let
        statusMap =
          Dict.fromList
          [ ("OK", (JD.succeed OK))
          , ("ERR", (JD.succeed ERR))
          , ("EXE", (JD.succeed EXE))
          ]

        innerDecoder =
          JD.map2
            Tuple.pair
            (JD.field "status"
              (JD.string |>
                JD.andThen
                  (\s -> Maybe.withDefault (JD.fail s) (Dict.get s statusMap))
              )
            )
            (JD.field "time" JD.string)
      in
        JD.keyValuePairs innerDecoder |>
        JD.andThen
          (\l ->
            case l of
              (id, (status, time)) :: [] ->
                JD.succeed <| (id, DeferredRequest status time)

              x -> JD.fail (Debug.toString x)
          )

    cmd id toSubMsg =
      Http.get
        { url = (conf.deferredResultBaseUri ++ "/" ++ id)
        , expect = expectJson toSubMsg JD.value
        }

    processNotification id req =
      Maybe.map
        (cmd id)
        (if req.status == EXE then Nothing else Dict.get id subs) |> -- get subscription if request is completed
      Maybe.map
        (Tuple.pair <| Model reqs (Dict.remove id subs) conf) |>  -- remove subscription, set deferred result cmd
      Maybe.withDefault
        ( Model (Dict.insert id req reqs) subs conf, Cmd.none ) -- insert notification

    processSubscription id toSubMsg =
      Dict.get id reqs |> -- get request
      Maybe.andThen
        (\req -> if req.status == EXE then Nothing else Just (id, toSubMsg)) |>  -- check if request is completed
      Maybe.map
        (uncurry cmd) |> -- get deferred result cmd
      Maybe.map
        (Tuple.pair <| Model (Dict.remove id reqs) subs conf) |> -- remove request, set deferred result cmd
      Maybe.withDefault
        ( Model reqs (Dict.insert id toSubMsg subs) conf, Cmd.none ) -- insert subscription

    resultCmd toResmsg res =
      do toResmsg <| res

    maybeSubscribeCmd subscr deferredResponse toResmsg =
      JD.decodeString conf.decoder deferredResponse |>
      Result.map
        (\did ->
          Cmd.batch
            [ resultCmd toResmsg True
            , do toMsg <| SubscribeMsg did subscr
            ]
        )

    maybeAskCmd deferredResponse toAskmsg toResmsg =
      if conf.isTimeoutErr deferredResponse then
        Ok <|
          Cmd.batch
            [ resultCmd toResmsg True
            , domsg <| toAskmsg conf.timeoutQuestion conf.deferredHeader
            ]
      else Err <| "Not timeout response: " ++ deferredResponse

    maybeSubscribeOrAsk subscr deferredResponse toAskmsg toResmsg =
      case maybeSubscribeCmd subscr deferredResponse toResmsg of
        Ok r -> Ok r

        Err _ ->
          maybeAskCmd deferredResponse toAskmsg toResmsg
  in
    case msg of
      UpdateMsg json ->
        Result.toMaybe
          (JD.decodeString notificationsDecoder json) |>
        Maybe.map
          (uncurry processNotification) |>
        Maybe.withDefault ( same, Cmd.none )

      SubscribeMsg id toSubMsg ->
        processSubscription id toSubMsg

      MaybeSubscribeMsg response subscr toResmsg ->
        ( same
        , maybeSubscribeCmd subscr response toResmsg |>
          Result.withDefault (resultCmd toResmsg False)
        )

      MaybeAskMsg response toAskmsg toResmsg ->
        ( same
        , maybeAskCmd response toAskmsg toResmsg |>
          Result.withDefault (resultCmd toResmsg False)
        )

      MaybeSubscribeOrAskMsg response subscr toAskmsg toResmsg ->
        ( same
        , maybeSubscribeOrAsk subscr response toAskmsg toResmsg |>
          Result.withDefault (resultCmd toResmsg False)
        )


{-| Subscription to web socket deferred request notifications.
-}
wsSubscriptions : Tomsg msg -> Model msg -> Sub msg
wsSubscriptions toMsg (Model _ _ { wsNotificationUri }) =
  Sub.batch
    [-- WebSocket.listen wsNotificationUri (toMsg << UpdateMsg)
    --, WebSocket.keepAlive wsNotificationUri
    ]
