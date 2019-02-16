port module Ports exposing
  ( windowScroll )


import Json.Decode as JD


port windowScroll: (JD.Value -> msg) -> Sub msg
