module Menu exposing
  ( Menu, Msg
  , init, urls, activeItem, activateMsg, update
  )


import Dict exposing (..)
import Task
import Browser exposing (..)
import Browser.Navigation as Nav
import Url exposing (..)


type Menu msg model
  = Menu (MenuInternal msg model)


type alias MenuInternal msg model =
  { key: Nav.Key
  , urls: List String
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
  = UrlClickedMsg UrlRequest
  | UrlChanged Url
  | ActionMsg String msg


type alias Tomsg msg = Msg msg -> msg


init:
  Nav.Key ->
  List String ->
  Init msg model -> Action msg model -> (String -> model -> model) ->
  Updater msg model ->
  Menu msg model
init key mitems activate action deactivate updater =
  Menu <| MenuInternal key mitems activate action deactivate updater Nothing


urls: Menu msg model -> List String
urls (Menu menu) = menu.urls


activeItem: Menu msg model -> Maybe String
activeItem (Menu menu) = menu.activeItem


urlRequestmsg: Tomsg msg -> (UrlRequest -> msg)
urlRequestmsg toMsg =
  toMsg << UrlClickedMsg


urlChangedmsg: Tomsg msg -> (Url -> msg)
urlChangedmsg toMsg =
  toMsg << UrlChanged


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
