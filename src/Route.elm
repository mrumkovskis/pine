module Route exposing
  ( Route, Routes, Msg
  , init, urlRequestMsg, urlChangedMsg
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
  , routes: Set (List String)
  , activePage: List String
  }


type Msg
  = UrlRequestMsg UrlRequest
  | UrlChangedMsg Url


type alias Tomsg msg = Msg -> msg


init: Nav.Key -> (Route -> msg) -> List String -> Routes msg
init key activateMsg routes =
  Routes key activateMsg (Set.fromList <| (List.map (String.split "/") routes)) []


urlRequestMsg: Tomsg msg -> (UrlRequest -> msg)
urlRequestMsg toMsg =
  toMsg << UrlRequestMsg


urlChangedMsg: Tomsg msg -> (Url -> msg)
urlChangedMsg toMsg =
  toMsg << UrlChangedMsg


update: Tomsg msg -> Msg -> Routes msg -> (Routes msg, Cmd msg)
update toMsg msg ({ key, activateMsg, routes, activePage } as model) =
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

        cmd =
          activateMsg >> Task.succeed >> Task.perform identity
      in
        if activePage == path then
          ( model, Cmd.none ) -- page is already active
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
              if r == activePage then
                ( model, Cmd.none ) -- page is already active
              else
                ( { model | activePage = r }
                , cmd <| Route (String.join "/" r) (List.drop (List.length r) path) <| params url.query
                )
            ) |>
          Maybe.withDefault ( { model | activePage = [] }, cmd <| Route "" path <| params url.query )
