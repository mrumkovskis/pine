module Menu exposing
  ( Menu, Msg, MenuItem
  , init, empty, items, item, isActive, activeItemUrl
  , urlRequestmsg, urlChangedmsg, update
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
  , menuItems: List (MenuItem msg model)
  , updater: msg -> model -> (model, Cmd msg)
  , menuUpdater: model -> Menu msg model -> model
  , activeItem: Maybe (MenuItem msg model)
  }


{- menu item fields - url, name, activate function, deactivate function -}
type MenuItem msg model
  = MenuItem String String (String -> Cmd msg) (model -> model)


type Msg msg model
  = UrlClickedMsg UrlRequest
  | UrlChangedMsg Url
  | ActionMsg (MenuItem msg model) msg


type alias Tomsg msg model = Msg msg model -> msg


init:
  Nav.Key -> List (MenuItem msg model) ->
  (msg -> model -> (model, Cmd msg)) -> (model -> Menu msg model -> model) -> Menu msg model
init key mitems updater menuUpdater =
  Menu <| MenuInternal key mitems updater menuUpdater Nothing


empty: Nav.Key -> Menu msg model
empty key =
  init key [] (\_ m -> (m, Cmd.none)) (\m _ -> m)


items: Menu msg model -> List (String, String)
items (Menu menu) =
  menu.menuItems |> List.map (\(MenuItem url name _ _) -> (url, name))


item: String -> String -> (String -> Cmd msg) -> (model -> model) -> MenuItem msg model
item = MenuItem


activeItemUrl: Menu msg model -> Maybe String
activeItemUrl (Menu { activeItem })  =
  activeItem |> Maybe.map (\(MenuItem url _ _ _) -> url)


isActive: String -> Menu msg model -> Bool
isActive murl (Menu menu) =
  menu.activeItem |>
  Maybe.map (\(MenuItem url _ _ _) -> murl == url) |>
  Maybe.withDefault False


urlRequestmsg: Tomsg msg model -> (UrlRequest -> msg)
urlRequestmsg toMsg =
  toMsg << UrlClickedMsg


urlChangedmsg: Tomsg msg model -> (Url -> msg)
urlChangedmsg toMsg =
  toMsg << UrlChangedMsg


update: Tomsg msg model -> Msg msg model -> Menu msg model -> model -> (model, Cmd msg)
update toMsg msg (Menu ({ menuItems, updater, menuUpdater, activeItem } as menu)) model =
  let
    maybeActivateItem it (newmod, cmd) =
      if cmd == Cmd.none then
        ( menuUpdater newmod (Menu { menu | activeItem = Just it }) |>
          (\nm ->
            activeItem |>
            Maybe.map
              (\(MenuItem _ _ _ deactivate as oldit) ->
                if oldit == it then nm else deactivate nm
              ) |>
            Maybe.withDefault nm
          )
        , Cmd.none
        )
      else ( newmod, Cmd.map (toMsg << ActionMsg it) cmd )

    itemAndParams path =
      menu.menuItems |>
      List.filter (\(MenuItem url _ _ _) -> String.startsWith url path) |>
      List.sortBy ((\(MenuItem url _ _ _) -> url) >> String.length >> negate) |>
      List.head |>
      Maybe.map
        (\(MenuItem url _ _ _ as it) -> ( it, String.dropLeft (String.length url) path ))

    urltostring path url =
      path ++
      (url.query |> Maybe.map ((++) "?") |> Maybe.withDefault "") ++
      (url.fragment |> Maybe.map ((++) "#") |> Maybe.withDefault "")
  in
    case msg of
      UrlClickedMsg urlRequest ->
        case urlRequest of
          Internal url ->
            ( model
            , url.fragment |>
              Maybe.andThen (\f -> if f == "nohref" then Just () else Nothing ) |>
              Maybe.map (always Cmd.none) |>
              Maybe.withDefault (Nav.pushUrl menu.key (Url.toString url))
            )

          External url ->
            (model, Nav.load url)

      UrlChangedMsg url ->
        itemAndParams url.path |>
        Maybe.map
          (\((MenuItem _ _ activate _ as it), params) ->
            maybeActivateItem it ( model, activate <| urltostring params url )
          ) |>
        Maybe.withDefault ( model, Cmd.none )

      ActionMsg it mmsg ->
        updater mmsg model |> maybeActivateItem it
