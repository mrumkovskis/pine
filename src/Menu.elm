module Menu exposing
  ( Menu, Msg
  , init, items, activeItem, activateMsg, update
  )


import Dict exposing (..)
import Task


type Menu msg model
  = Menu (MenuInternal msg model)


type alias MenuInternal msg model =
  { items: List String
  , activate: Init msg model
  , action: Action msg model
  , deactivate: String -> model -> model
  , updater: Updater msg model
  , activeItem: Maybe String
  }


type alias Init msg model = (msg -> msg) -> String -> model -> (model, Cmd msg)


type alias Action msg model = (msg -> msg) -> msg -> model -> (model, Cmd msg)


type alias Updater msg model = Menu msg model -> model -> model


type Msg msg
  = ActivateMsg String
  | ActionMsg String msg


type alias Tomsg msg = Msg msg -> msg


init:
  List String ->
  Init msg model -> Action msg model -> (String -> model -> model) ->
  Updater msg model ->
  Menu msg model
init mitems activate action deactivate updater =
  Menu <| MenuInternal mitems activate action deactivate updater Nothing


items: Menu msg model -> List String
items (Menu menu) = menu.items


activeItem: Menu msg model -> Maybe String
activeItem (Menu menu) = menu.activeItem


activateMsg: Tomsg msg -> String -> msg
activateMsg toMsg name =
  toMsg <| ActivateMsg name


update: Tomsg msg -> Msg msg -> Menu msg model -> model -> (model, Cmd msg)
update toMsg msg (Menu ({ activate, action, deactivate, updater } as menu)) model =
  let
    activateItem newmod name =
      let newMenu = { menu | activeItem = Just name } in
        menu.activeItem |>
        Maybe.andThen (\oldname -> if oldname == name then Nothing else Just oldname) |>
        Maybe.map (\oldname -> deactivate oldname newmod) |>
        Maybe.map (menu.updater <| Menu newMenu) |>
        Maybe.withDefault (menu.updater (Menu newMenu) newmod)

    maybeActivateItem name (newmod, cmd) =
      if cmd == Cmd.none then
        ( activateItem newmod name, Cmd.none )
      else ( newmod, cmd )
  in
    case msg of
      ActivateMsg name ->
        activate (toMsg << ActionMsg name) name model |>
        (maybeActivateItem name)

      ActionMsg name itemmsg ->
        action (toMsg << ActionMsg name) itemmsg model |>
        (maybeActivateItem name)
