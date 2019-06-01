module ScrollEvents exposing
  ( Model, Msg, StickyElPos, Tomsg, init
  , subscribeToScrollIntoVisibility, subscribeToStick
  , process, visibilitySub, stickySub, update, subscriptions
  )


import Ports exposing (windowScroll)

import Browser.Events as Ev
import Browser.Dom as Dom
import Dict exposing (Dict)
import Task

import Debug exposing (log, toString)


type alias StickyElPos =
  { top: Float
  , left: Float
  }


type Model msg =
  Model
    { stickToElId: String
    , visibilitySubscriptions: Dict String msg
    , stickSubscriptions: Dict String (String -> Maybe StickyElPos -> msg)
    }


type Msg msg
  = CheckVisibilityMsg String msg
  | CheckStickyMsg String String (Maybe StickyElPos -> msg)
  | WindowScrollOrResizeMsg
  | SubscribeVisibilityMsg String msg
  | SubscribeStickToMsg String (String -> Maybe StickyElPos -> msg)
  | UnsubscribeVisibilityMsg String
  | UnsubscribeStickToMsg String
  | NoOp (Maybe Dom.Error)


type alias Tomsg msg = Msg msg -> msg


init: String -> Dict String (String -> Maybe StickyElPos -> msg) -> Dict String msg -> Model msg
init stickToElId stickSubs visibilitySubs =
  Model
    { stickToElId = stickToElId
    , visibilitySubscriptions = visibilitySubs
    , stickSubscriptions = stickSubs
    }


subscribeToScrollIntoVisibility: Tomsg msg -> String -> msg -> Cmd msg
subscribeToScrollIntoVisibility toMsg elId msg =
  Task.perform (toMsg << SubscribeVisibilityMsg elId) <| Task.succeed msg


subscribeToStick: Tomsg msg -> String -> (String -> Maybe StickyElPos -> msg) -> Cmd msg
subscribeToStick toMsg elId toStickPosmsg =
  Task.perform (toMsg << SubscribeStickToMsg elId) <| Task.succeed toStickPosmsg


process: Tomsg msg -> Msg msg -> Cmd msg
process toMsg msg =
  case msg of
    CheckVisibilityMsg id vmsg ->
      Dom.getElement id |>
      Task.map
        (\ { viewport, element } ->
          if element.y <= viewport.y + viewport.height then
            vmsg
          else
            toMsg <| NoOp Nothing
        ) |>
      Task.onError (\e -> Task.succeed <| toMsg <| NoOp <| Just e) |>
      Task.perform identity

    CheckStickyMsg stickToId id stickMsg ->
      Task.sequence [ Dom.getElement stickToId, Dom.getElement id ] |>
      Task.map
        (\ res ->
          case res of
            [ stickToPos, pos ] ->
              (stickToPos.element, stickToPos.viewport, pos.element) |>
              (\(upper, sviewport, lower) ->
                if lower.y < upper.y + upper.height then
                  stickMsg <|
                    Just
                      { top = upper.height + upper.y - sviewport.y
                      , left = lower.x
                      }
                else
                  stickMsg Nothing
              )
            _ -> toMsg <| NoOp Nothing
        ) |>
      Task.onError (\e -> Task.succeed <| toMsg <| NoOp <| Just e) |>
      Task.perform identity

    _ -> Cmd.none

update: Tomsg msg -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update toMsg msg (Model ({ stickToElId, visibilitySubscriptions, stickSubscriptions } as mi) as same) =
  case msg of
    CheckVisibilityMsg id vmsg ->
      Dom.getElement id |>
      Task.map
        (\ { viewport, element } ->
          if element.y <= viewport.y + viewport.height then
            vmsg
          else
            toMsg <| NoOp Nothing
        ) |>
      Task.onError (\e -> Task.succeed <| toMsg <| NoOp <| Just e) |>
      Task.perform identity |>
      Tuple.pair same

    CheckStickyMsg stickToId id stickMsg ->
      Task.sequence [ Dom.getElement stickToId, Dom.getElement id ] |>
      Task.map
        (\ res ->
          case res of
            [ stickToPos, pos ] ->
              (stickToPos.element, stickToPos.viewport, pos.element) |>
              (\(upper, sviewport, lower) ->
                if lower.y < upper.y + upper.height then
                  stickMsg <|
                    Just
                      { top = upper.height + upper.y - sviewport.y
                      , left = lower.x
                      }
                else
                  stickMsg Nothing
              )
            _ -> toMsg <| NoOp Nothing
        ) |>
      Task.onError (\e -> Task.succeed <| toMsg <| NoOp <| Just e) |>
      Task.perform identity |>
      Tuple.pair same

    WindowScrollOrResizeMsg ->
      let
        visibilityCmds =
          visibilitySubscriptions |>
          Dict.foldl
            (\id submsg cmds ->
              ( Dom.getElement id |>
                Task.map
                  (\ { viewport, element } ->
                    if element.y <= viewport.y + viewport.height then
                      submsg
                    else
                      toMsg <| NoOp Nothing
                  ) |>
                Task.onError (\e -> Task.succeed <| toMsg <| NoOp <| Just e) |>
                Task.perform identity
              ) :: cmds
            )
            []

        stickCmds =
          if Dict.isEmpty stickSubscriptions then
            []
          else
            stickSubscriptions |>
            Dict.foldl
              (\id submsg cmds ->
                ( Task.sequence [ Dom.getElement stickToElId, Dom.getElement id ] |>
                  Task.map
                    (\ res ->
                      case res of
                        [ stickToPos, pos ] ->
                          (stickToPos.element, stickToPos.viewport, pos.element) |>
                          (\(upper, sviewport, lower) ->
                            if lower.y < upper.y + upper.height then
                              submsg id <|
                                Just
                                  { top = upper.height + upper.y - sviewport.y
                                  , left = lower.x
                                  }
                            else
                              submsg id <| Nothing
                          )
                        _ -> toMsg <| NoOp Nothing
                    ) |>
                  Task.onError (\e -> Task.succeed <| toMsg <| NoOp <| Just e) |>
                  Task.perform identity
                ) :: cmds
              )
              []
      in
        ( same, Cmd.batch <| visibilityCmds ++ stickCmds )

    SubscribeVisibilityMsg elId vismsg ->
      ( Model
          { mi |
            visibilitySubscriptions =
              visibilitySubscriptions |> Dict.insert elId vismsg
          }
      , Cmd.none
      )

    SubscribeStickToMsg elId toStickPosmsg ->
      ( Model
          { mi |
            stickSubscriptions =
              stickSubscriptions |> Dict.insert elId toStickPosmsg
          }
      , Cmd.none
      )

    UnsubscribeVisibilityMsg elId ->
      ( Model
          { mi |
            visibilitySubscriptions =
              visibilitySubscriptions |> Dict.remove elId
          }
      , Cmd.none
      )

    UnsubscribeStickToMsg elId ->
      ( Model
          { mi |
            stickSubscriptions =
              stickSubscriptions |> Dict.remove elId
          }
      , Cmd.none
      )

    NoOp maybeErr ->
      ( same, Cmd.none )


subscriptions: Tomsg msg -> Model msg -> Sub msg
subscriptions toMsg (Model { visibilitySubscriptions, stickSubscriptions }) =
  if Dict.isEmpty visibilitySubscriptions && Dict.isEmpty stickSubscriptions then
    Sub.none
  else
    Sub.batch
      [ windowScroll (\_ -> toMsg WindowScrollOrResizeMsg)
      , Ev.onResize (\_ _ -> toMsg WindowScrollOrResizeMsg)
      ]


visibilitySub: Tomsg msg -> String -> msg -> Sub msg
visibilitySub toMsg id msg =
  Sub.batch
    [ windowScroll (\_ -> toMsg <| CheckVisibilityMsg id msg)
    , Ev.onResize (\_ _ -> toMsg <| CheckVisibilityMsg id msg)
    ]


stickySub: Tomsg msg -> String -> String -> (Maybe StickyElPos -> msg) -> Sub msg
stickySub toMsg stickToElId id toStickmsg =
  Sub.batch
    [ windowScroll (\_ -> toMsg <| CheckStickyMsg stickToElId id toStickmsg)
    , Ev.onResize (\_ _ -> toMsg <| CheckStickyMsg stickToElId id toStickmsg)
    ]
