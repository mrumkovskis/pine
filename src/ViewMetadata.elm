module ViewMetadata exposing
  ( View
  , Field
  , fetchMetadata
  , field
  , initField
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
import Task exposing (..)
import Dict exposing (..)
import Url exposing (percentEncode)


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


{-| Fetch view metadata.
    fetchMetadata toMsg "/metadata" "view-name"
-}
fetchMetadata: (Result HttpError (Dict String View) -> msg) -> String -> String -> Cmd msg
fetchMetadata toMsg urlBase viewName =
  Task.attempt toMsg <| fetchMetadataTask urlBase viewName


fetchMetadataTask: String -> String -> Task HttpError (Dict String View)
fetchMetadataTask urlBase viewName =
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

    task name =
      Http.task
        { method = "GET"
        , headers = []
        , url = urlBase ++ "/" ++ percentEncode name
        , body = Http.emptyBody
        , resolver = resolveJson viewDecoder
        , timeout = Nothing
        }

    tasks result =
      Dict.filter (\_ v -> v == Nothing) result |>
      (\rest ->
        if Dict.isEmpty rest then
          Task.succeed result
        else
          rest |>
          Dict.keys |>
          List.map task |>
          Task.sequence |>
          Task.map
            ( List.foldl
                (\view res ->
                  (.fields >> List.filter .isComplexType >> List.map .typeName) view |>
                  List.foldl
                    (\view_name res1 ->
                      Dict.get view_name res1 |>
                      Maybe.map (\_ -> res1) |>
                      Maybe.withDefault (Dict.insert view_name Nothing res1)
                    )
                    (Dict.insert view.typeName (Just view) res)
                )
                result
            ) |>
          Task.andThen tasks
      )
  in
    tasks (Dict.fromList [(viewName, Nothing)]) |>
    Task.map
      ( Dict.foldl
          (\n mv r -> mv |> Maybe.map (\v -> Dict.insert n v r) |> Maybe.withDefault r)
          (Dict.empty)
      )


field: String -> View -> Maybe Field
field name view =
  view.fields |>
  List.filter (\f -> f.name == name) |>
  List.head


initField: String -> String -> Field
initField name label =
  { name = name
  , label = label
  , typeName = ""
  , jsonType = "string"
  , nullable = True
  , required = False
  , visible = True
  , sortable = False
  , enum = Nothing
  , length = Nothing
  , totalDigits = Nothing
  , fractionDigits = Nothing
  , isCollection = False
  , isComplexType = False
  , comments = ""
  , refViewName = Nothing
  }
