module Route exposing
  ( Route, Routes
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


type alias Route =
  { route: String
  , path: List String
  , params: JM.SearchParams
  }


type alias Routes msg =
  { key: Nav.Key
  , activateMsg: Route -> msg
  , routes: Set String
  , activePage: String
  }


type Msg
  = UrlRequestMsg UrlRequest
  | UrlChangedMsg Url


type alias Tomsg msg = Msg -> msg


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
          Utils.decodeUrlPath url.path |>
          String.join "/"

        cmd =
          activateMsg >> Task.succeed >> Task.perform identity
      in
        if String.startsWith activePage path then
          ( model, Cmd.none ) -- page is already active, url is changed for history purposes
        else if Set.member path routes then
          ( { model | activePage = path }, cmd <| Route path [] [] )
        else
          let
            route =
              Set.toList routes |>
              List.filter (\r -> String.startsWith r path) |>
              List.head |>
              Maybe.map
                (\r ->
                  Route
                    r
                    ( String.dropLeft (String.length r + 1) path |>
                      String.split "/" |>
                      List.filter (String.length >> (/=) 0)
                    )
                    (url.query |> Maybe.map Utils.decodeHttpQuery |> Maybe.withDefault [])
                ) |>
              Maybe.withDefault (Route (Url.toString url) [] [])
          in
            ( { model | activePage = route.route}, cmd route )
