module AddressModel exposing
  ( Address, decoder, resolveAddressDecoder, initAddress, init )


{-| Address service according to [`addresses`](https://github.com/mrumkovskis/addresses)
This is relevant for LV addresses.

@docs Address, decoder, resolveAddressDecoder, initAddress, init
-}


import Json.Decode as JD
import Json.Encode as JE
import Http

import JsonModel as JM
import Ask
import Select
import Utils


{-| Address structure (LV) -}
type alias Address =
 { code: Maybe Int
 , address: String
 , zipCode: Maybe String
 , coordX: Maybe Float
 , coordY: Maybe Float
 , pilCode: Maybe Int
 , novCode: Maybe Int
 , pagCode: Maybe Int
 , pilName: Maybe String
 , novName: Maybe String
 , pagName: Maybe String
 }


{-| Address decoder.
-}
decoder: JD.Decoder Address
decoder =
  let
    opt = Utils.optField
  in
    JD.map5
      Address
      (opt "code" JD.int)
      (JD.field "address" JD.string)
      (opt "zipCode" JD.string)
      (opt "coordX" JD.float)
      (opt "coordY" JD.float)
    |>
    JD.andThen
      (\a ->
        JD.map6
          a
          (opt "pilCode" JD.int)
          (opt "novCode" JD.int)
          (opt "pagCode" JD.int)
          (opt "pilName" JD.string)
          (opt "novName" JD.string)
          (opt "pagName" JD.string)
      )


{-| Resolve address decoder. Resolved address has structure like this:
    `
    {
      "address": "Vīlandes iela 7 - 9, Rīga",
      "resolvedAddress": {
        "dzvCode": 113136060,
        "ielCode": 100314142,
        "ielName": "Vīlandes iela",
        "nltName": "7",
        "coordX": 506183.961,
        "pilCode": 100003003,
        "zipCode": "LV-1010",
        "dzvName": "9",
        "nltCode": 102023651,
        "code": 113136060,
        "typ": 104,
        "address": "Vīlandes iela 7 - 9\nRīga",
        "coordY": 312966.488,
        "pilName": "Rīga"
      }
    }
    `
-}
resolveAddressDecoder: JD.Decoder Address
resolveAddressDecoder =
  JD.map2
    Tuple.pair
    (JD.field "address" JD.string)
    (JD.maybe <| JD.field "resolvedAddress" decoder) |>
  JD.map
    (\(a, mra) ->
      mra |> Maybe.map (\ra -> { ra | address = a }) |> Maybe.withDefault (initAddress a)
    )


{-| Initialize address with text (without structure).
-}
initAddress: String -> Address
initAddress address =
  Address Nothing address Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


{-| Initialize [`Select`](Select)
backed by [`addresses`](https://github.com/mrumkovskis/addresses) service.
-}
init: String -> Select.Tomsg msg JM.JsonValue -> Ask.Tomsg msg -> String -> (String -> msg) -> model -> (Select.SelectModel msg JM.JsonValue, Cmd msg)
init uri toSelectmsg toMessagemsg search toAddressmsg _ =
  ( Select.init
      (JM.initList
        "/metadata"
        uri
        "address"
        (JD.field "address" JD.string |> JD.map JM.JsString)
        (always JE.null)
        toMessagemsg
      )
      "search"
      search
      (JM.jsonString "" >> Maybe.withDefault "<nav>" >> toAddressmsg)
      (JM.jsonString "" >> Maybe.withDefault "<nav>")
  , Cmd.none
  )