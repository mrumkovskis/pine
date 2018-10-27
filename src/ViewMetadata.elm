module ViewMetadata exposing
  ( View
  , Field
  , Msg (..)
  , fetchMetadata
  )


{-| Elm represantation of [mojoz](https://github.com/guntiso/mojoz) view metadata.
`fetchMetadata` command gets view's metadata from server.

# Types
@docs View, Field, Msg

# Commands
@docs fetchMetadata
-}

import Utils exposing (..)

import Json.Decode as JD
import Http
import Set exposing (Set)


{-| View definition.
-}
type alias View =
  { typeName: String
  , fields: List Field
  }


{-| Field definition.
-}
type alias Field =
  { name: String
  , label: String
  , typeName: String
  , isCollection: Bool
  , isComplexType: Bool
  , nullable: Bool
  , visible: Bool
  , sortable: Bool
  , comments: String
  }


{-| metadata message with view and children name set -}
type Msg = ViewMetadataMsg (Result Http.Error View) (Set String)


type MsgInternal = ViewSingleMetadataMsg (Result Http.Error View)


{-| Fetch view metadata.
    fetchMetadata "/metadata/view-name"
-}
fetchMetadata: String -> Cmd Msg
fetchMetadata uri =
  (Http.send ViewSingleMetadataMsg (Http.get uri viewDecoder)) |>
  Cmd.map
    (\(ViewSingleMetadataMsg res) ->
      ViewMetadataMsg
        res
        ( Result.toMaybe res |>
          Maybe.map .fields |>
          Maybe.map (List.filter .isComplexType) |>
          Maybe.map (List.map .typeName) |>
          Maybe.map Set.fromList |>
          Maybe.withDefault Set.empty
        )
    )


-- decoders

viewDecoder: JD.Decoder View
viewDecoder =
  JD.map2
    View
    (JD.field "name" JD.string)
    (JD.field "fields" (JD.list fieldDecoder))


fieldDecoder: JD.Decoder Field
fieldDecoder =
  let
    stringFieldDecoder name = JD.field name JD.string

    optionalStringFieldDecoder name =
      JD.field name (JD.oneOf [JD.string, JD.null ""])

    boolFieldDecoder name = JD.field name JD.bool
  in
    JD.map8
      Field
      (stringFieldDecoder "name")
      (stringFieldDecoder "label")
      (stringFieldDecoder "type")
      (boolFieldDecoder "isCollection")
      (boolFieldDecoder "isComplexType")
      (boolFieldDecoder "nullable")
      (boolFieldDecoder "visible")
      (boolFieldDecoder "sortable") |>
    JD.andThen
      (flip JD.map <| optionalStringFieldDecoder "comments")
