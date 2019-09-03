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
onNavigation toMsg isActive =
  let
    mapper key =
      case key of
        38 -> JD.succeed Up -- arrow up

        40 -> JD.succeed Down -- arrow down

        27 -> if isActive then JD.succeed Esc else JD.fail "Not active select box"  -- escape

        13 -> if isActive then JD.succeed Select else JD.fail "Not active select box" -- enter

        _ -> JD.fail "Not selection key"

    decoder = keyCode |> JD.andThen mapper |> JD.map toMsg
  in
    -- on "keydown" decoder, use preventDefaultOn to stop form submission
    preventDefaultOn
      "keydown"
      (JD.map (\k -> (k, True)) decoder)
