module SelectEvents exposing
  ( Msg (..)
  , onNavigation
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD


type Msg
  = Up
  | Down
  | Esc
  | Select


onNavigation: (Msg -> msg) -> Bool -> Attribute msg
onNavigation toMsg preventDefault =
  let
    mapper key =
      case key of
        38 -> JD.succeed Up -- arrow up

        40 -> JD.succeed Down -- arrow down

        27 -> JD.succeed Esc -- escape

        13 -> JD.succeed Select -- enter

        _ -> JD.fail "Not selection/navigation key"

    decoder = keyCode |> JD.andThen mapper |> JD.map toMsg
  in
    -- on "keydown" decoder, may use preventDefaultOn to stop form submission
    preventDefaultOn
      "keydown"
      (JD.map (\k -> (k, preventDefault)) decoder)
