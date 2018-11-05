module AddressModel exposing
  ( Address, decoder, initAddress, init )


{-| Address service according to [`addresses`](https://github.com/mrumkovskis/addresses)
This is relevant for LV addresses.

@docs Address, decoder, initAddress, init
-}


import Json.Decode as JD
import Json.Encode as JE
import JsonModel as JM
import Ask
import DeferredRequests as DR
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


{-| Initialize address with text (without structure).
-}
initAddress: String -> Address
initAddress address =
  Address Nothing address Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


{-| Initialize [`Select`](Select)
backed by [`addresses`](https://github.com/mrumkovskis/addresses) service.
-}
init:
  String ->
  Ask.Tomsg msg ->
  DR.Tomsg msg ->
  String ->
  (String -> msg) ->
  Select.SelectModel msg String
init uri toMessagemsg toDeferredmsg search toAddressmsg =
  Select.init
    (JM.initList
      "/metadata"
      uri
      "address"
      (JD.field "address" JD.string)
      (always JE.null)
      toMessagemsg |>
      JM.defaultDeferredSettings toDeferredmsg "20s"
    )
    "search"
    search
    toAddressmsg
    identity
