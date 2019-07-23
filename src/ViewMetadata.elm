module ViewMetadata exposing
  ( View
  , Field
  , Msg (..)
  , fetchMetadata
  , field
  )


{-| Elm represantation of [mojoz](https://github.com/guntiso/mojoz) view metadata.
`fetchMetadata` command gets view's metadata from server.

# Types
@docs View, Field, Msg

# Commands
@docs fetchMetadata, field
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
  , filter: List Field
  }


{-| Field definition.
-}
type alias Field =
  { name: String
  , label: String
  , typeName: String
  , jsonType: String
  , nullable: Bool
  , required: Bool
  , visible: Bool
  , sortable: Bool
  , enum: Maybe (List String)
  , length: Maybe Int
  , totalDigits: Maybe Int
  , fractionDigits: Maybe Int
  , isCollection: Bool
  , isComplexType: Bool
  , comments: String
  , refViewName: Maybe String
  }


{-| metadata message with view and children name set -}
type Msg = ViewMetadataMsg (Result Http.Error View) (Set String)


type MsgInternal = ViewSingleMetadataMsg (Result Http.Error View)


{-| Fetch view metadata.
    fetchMetadata "/metadata/view-name"
-}
fetchMetadata: String -> Cmd Msg
fetchMetadata uri =
  let
    viewDecoder =
      JD.map3
        View
        (JD.field "name" JD.string)
        (JD.field "fields" <| JD.list fieldDecoder)
        (JD.field "filter" <| JD.list filterDecoder)

    stringFieldDecoder name = JD.field name JD.string

    optionalStringFieldDecoder name =
      JD.field name (JD.oneOf [JD.string, JD.null ""])

    maybeStringFieldDecoder name =
      JD.maybe <| JD.field name <| JD.oneOf [ JD.string, JD.fail "knipis" ]

    boolFieldDecoder name = JD.field name JD.bool

    optionalIntFieldDecoder name =
      JD.maybe <| JD.field name  JD.int

    fieldDecoder =
      JD.map8
        Field
        (stringFieldDecoder "name")
        (optionalStringFieldDecoder "label")
        (stringFieldDecoder "type")
        (stringFieldDecoder "jsonType")
        (boolFieldDecoder "nullable")
        (boolFieldDecoder "required")
        (boolFieldDecoder "visible")
        (boolFieldDecoder "sortable") |>
      JD.andThen
        (\v ->
          JD.map8
            v
            (JD.maybe <| JD.field "enum" <| JD.list JD.string)
            (optionalIntFieldDecoder "length")
            (optionalIntFieldDecoder "totalDigits")
            (optionalIntFieldDecoder "fractionDigits")
            (boolFieldDecoder "isCollection")
            (boolFieldDecoder "isComplexType")
            (optionalStringFieldDecoder "comments")
            (maybeStringFieldDecoder "refViewName")
        )

    filterDecoder =
      JD.map8
        Field
        (stringFieldDecoder "name")
        (optionalStringFieldDecoder "label")
        (stringFieldDecoder "type")
        (stringFieldDecoder "jsonType")
        (boolFieldDecoder "nullable")
        (boolFieldDecoder "required")
        (JD.succeed True)
        (JD.succeed False) |>
      JD.andThen
        (\v ->
          JD.map8
            v
            (JD.maybe <| JD.field "enum" <| JD.list JD.string)
            (JD.succeed Nothing)
            (JD.succeed Nothing)
            (JD.succeed Nothing)
            (JD.succeed False)
            (JD.succeed False)
            (JD.succeed "")
            (maybeStringFieldDecoder "refViewName")
        )
  in
    (Http.get { url = uri, expect = Http.expectJson ViewSingleMetadataMsg viewDecoder }) |>
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


field: String -> View -> Maybe Field
field name view =
  view.fields |>
  List.filter (\f -> f.name == name) |>
  List.head
