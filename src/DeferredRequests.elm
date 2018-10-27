module DeferredRequests exposing
  ( DeferredRequest, DeferredStatus (..), Tomsg, Model, Msg
  , init, update, subscribeCmd, maybeSubscribeCmd, wsSubscriptions
  , requests, subscriptions
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


import Utils exposing (uncurry)

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


{-| Msg constructor type alias
-}
type alias Tomsg msg = Msg msg -> msg


{-| Subscription to deferred result.
-}
type alias Subscription msg = (Result Http.Error JD.Value) -> msg


type alias Config =
  { deferredResultBaseUri: String
  , wsNotificationUri: String
  }


{-| Model
-}
type Model msg = Model (Dict String DeferredRequest) (Dict String (Subscription msg)) Config


{-| Messages. Sent to update deferred request statuses and subscribe to deferred results.
-}
type Msg msg
  = UpdateMsg String
  | SubscribeMsg String (Subscription msg)


{-| Creates model. First argument specifies base uri where to get ready results
(real uri appends requests id to the base), second argument is web socket notification
uri of request execution progress.
-}
init: String -> String -> Model msg
init deferredResultBaseUri wsNotificationUri =
  Model Dict.empty Dict.empty <| Config deferredResultBaseUri wsNotificationUri


{-| Get active requests. Key is request id.
-}
requests: Model msg -> Dict String DeferredRequest
requests (Model r _ _) = r


{-| Get active subscriptions. List element is request id.
-}
subscriptions: Model msg -> List String
subscriptions (Model _ s _) = Dict.keys s


{-| Subscribe to deferred result. Subscription message is sent
when result is ready (OK or ERR).
-}
subscribeCmd: Tomsg msg -> Subscription msg -> String -> Cmd msg
subscribeCmd toMsg subscription deferredRequestId =
  Task.perform toMsg <| Task.succeed <| SubscribeMsg deferredRequestId subscription


{-| Maybe subscribe to deferred result. Subscription is successful if
third argument `deferredResponse` corresponds following json pattern:

    `{"deferred":"w4OIXzaWt2437gZ1fqxMsFAHXVA"}`
-}
maybeSubscribeCmd: Tomsg msg -> Subscription msg -> String -> Maybe (Cmd msg)
maybeSubscribeCmd toMsg subscription deferredResponse =
  let
    decoder = JD.field "deferred" JD.string
  in
    (Result.toMaybe <| JD.decodeString decoder deferredResponse) |>
    (Maybe.map <| subscribeCmd toMsg subscription)


{-| Model update.
-}
update: Tomsg msg -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update toMsg msg (Model reqs subs conf) =
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
      Http.send toSubMsg <|
        Http.get (conf.deferredResultBaseUri ++ "/" ++ id) JD.value

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


{-| Subscription to web socket deferred request notifications.
-}
wsSubscriptions : Tomsg msg -> Model msg -> Sub msg
wsSubscriptions toMsg (Model _ _ { wsNotificationUri }) =
  Sub.batch
    [-- WebSocket.listen wsNotificationUri (toMsg << UpdateMsg)
    --, WebSocket.keepAlive wsNotificationUri
    ]
