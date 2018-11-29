module Menu exposing
  ( Menu, Msg
  , init, items, activeItem, navKey, urlRequestmsg, urlChangedmsg, update
  )


import Dict exposing (..)
import Task
import Browser exposing (..)
import Browser.Navigation as Nav
import Url exposing (..)

import Debug exposing (log)


type Menu msg model
  = Menu (MenuInternal msg model)


type alias MenuInternal msg model =
  { key: Nav.Key
  , items: List (String, String) -- url, name
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
  | UrlChangedMsg Url
  | ActionMsg String msg


type alias Tomsg msg = Msg msg -> msg


init:
  Nav.Key ->
  List (String, String) ->
  Init msg model -> Action msg model -> (String -> model -> model) ->
  Updater msg model ->
  Menu msg model
init key mitems activate action deactivate updater =
  Menu <| MenuInternal key mitems activate action deactivate updater Nothing


items: Menu msg model -> List (String, String)
items (Menu menu) = menu.items


activeItem: Menu msg model -> Maybe String
activeItem (Menu menu) = menu.activeItem


navKey: Menu msg model -> Nav.Key
navKey (Menu { key }) =
  key

urlRequestmsg: Tomsg msg -> (UrlRequest -> msg)
urlRequestmsg toMsg =
  toMsg << UrlClickedMsg


urlChangedmsg: Tomsg msg -> (Url -> msg)
urlChangedmsg toMsg =
  toMsg << UrlChangedMsg


update: Tomsg msg -> Msg msg -> Menu msg model -> model -> (model, Cmd msg)
update toMsg msg (Menu ({ activate, action, deactivate, updater } as menu)) model =
  let
    activateItem newmod url =
      let newMenu = { menu | activeItem = Just url } in
        menu.activeItem |>
        Maybe.andThen (\oldname -> if oldname == url then Nothing else Just oldname) |>
        Maybe.map (\oldname -> deactivate oldname newmod) |>
        Maybe.map (menu.updater <| Menu newMenu) |>
        Maybe.withDefault (menu.updater (Menu newMenu) newmod)

    maybeActivateItem url (newmod, cmd) =
      if cmd == Cmd.none then
        ( activateItem newmod url, Cmd.none )
      else ( newmod, cmd )
  in
    case msg of
      UrlClickedMsg urlRequest ->
        case urlRequest of
          Internal url ->
            ( model
            , url.fragment |>
              Maybe.andThen (\f -> if f == "nogo" then Just () else Nothing ) |>
              Maybe.map (always Cmd.none) |>
              Maybe.withDefault (Nav.pushUrl menu.key (Url.toString url))
            )

          External url ->
            (model, Nav.load url)

      UrlChangedMsg url ->
        activate (toMsg << ActionMsg url.path) url.path model |>
        (maybeActivateItem url.path)

      ActionMsg url itemmsg ->
        action (toMsg << ActionMsg url) itemmsg model |>
        (maybeActivateItem url)
