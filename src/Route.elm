module Route exposing
  ( Route, Routes, Msg
  , init, activeRoute
  , urlRequestMsg, urlChangedMsg
  , update
  )


import JsonModel as JM
import Utils

import Set exposing (Set)
import Browser.Navigation as Nav
import Browser exposing (..)
import Url exposing (..)
import Task

import Debug


type alias Route =
  { route: String
  , path: List String
  , params: JM.SearchParams
  }


type alias Routes msg =
  { key: Nav.Key
  , activateMsg: Route -> msg
  , syncMsg: Route -> msg
  , routes: Set (List String)
  , activePage: List String
  }


type Msg
  = UrlRequestMsg UrlRequest
  | UrlChangedMsg Url


type alias Tomsg msg = Msg -> msg


init: Nav.Key -> (Route -> msg) -> (Route -> msg) -> List String -> Routes msg
init key activateMsg syncMsg routes =
  Routes key activateMsg syncMsg (Set.fromList <| (List.map (String.split "/") routes)) []


urlRequestMsg: Tomsg msg -> (UrlRequest -> msg)
urlRequestMsg toMsg =
  toMsg << UrlRequestMsg


urlChangedMsg: Tomsg msg -> (Url -> msg)
urlChangedMsg toMsg =
  toMsg << UrlChangedMsg


activeRoute: Routes msg -> String
activeRoute { activePage } =
  String.join "/" activePage


update: Tomsg msg -> Msg -> Routes msg -> (Routes msg, Cmd msg)
update toMsg msg ({ key, activateMsg, syncMsg, routes, activePage } as model) =
  case msg of
    UrlRequestMsg urlRequest ->
      case urlRequest of
        Internal url ->
          ( model, Nav.pushUrl key <| Url.toString url )

        External url ->
          ( model, Nav.load url )

    UrlChangedMsg url ->
      let
        path =
          Utils.decodeUrlPath url.path

        params query =
          query |> Maybe.map Utils.decodeHttpQuery |> Maybe.withDefault []

        activateCmd =
          activateMsg >> Task.succeed >> Task.perform identity

        syncCmd =
          syncMsg >> Task.succeed >> Task.perform identity
      in
        if activePage == path then
          ( model, syncCmd <| Route (String.join "/" path) [] <| params url.query ) -- page is already active - sync
        else
          List.foldl
            (\rt (c, r) ->
              let
                x = Utils.eqElCount rt path
              in
                if x > c && x == List.length rt then
                  ( x, Just rt )
                else (c, r)
            )
            (0, Nothing)
            (Set.toList routes) |>
          Tuple.second |>
          Maybe.map
            (\r ->
              let
                route =
                  Route
                    (String.join "/" r)
                    (List.drop (List.length r) path) <|
                    params url.query
              in
                if r == activePage then
                  ( model, syncCmd route ) -- page is already active - sync
                else
                  ( { model | activePage = r }, activateCmd route )
            ) |>
          Maybe.withDefault
            ( { model | activePage = [] }
            , activateCmd <| Route "" path <| params url.query
            )
