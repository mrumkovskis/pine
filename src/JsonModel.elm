module JsonModel exposing
  ( -- types
    Model (..), JsonValue (..), Msg, Tomsg, ListModel, ListMsg, JsonListModel
  , JsonListMsg, FormModel, FormMsg, JsonFormModel, JsonFormMsg
  , Path (..), SearchParams, Decoder, Encoder, DataFetcher, CountFetcher
  -- initialization, configuration
  , initJsonList, initList, initJsonForm, initJsonQueryForm, initJsonValueForm, initForm, initQueryForm
  , listDecoder, formDecoder, countBaseUri, pageSize, countDecoder, idParam
  , offsetLimitParams , enableDeferred, enableDeferredWithTimeout, dataFetcher, countFetcher
  , setData
  -- data examination
  , data, progress, isProgress, completed, count, isEmpty, id, searchPars
  -- metadata examination
  , conf, columnNames, visibleColumnNames, fieldNames, visibleFieldNames
  , columnLabels, visibleColumnLabels, fieldLabels, visibleFieldLabels
  , field
  -- utility functions
  , jsonDataDecoder, jsonString, jsonInt, jsonFloat, jsonBool, jsonList, jsonEditor, jsonReader
  , jsonEdit, jsonValueToString, stringToJsonValue, searchParsFromJson, flattenJsonForm
  , pathDecoder, pathEncoder, reversePath
  , isInitialized, notInitialized, ready
  -- commands
  , fetch,fetchMsg, fetchFromStart, fetchFromStartMsg, fetchDeferred, fetchDeferredFromStart, fetchCount
  , fetchCountDeferred, fetchMetadata, set, edit, save, create, delete
  , httpDataFetcher, httpCountFetcher, enumFetcher
  -- model updater
  , update
  )

{-| Json rest service client. Service metadata is based on [mojoz](https://github.com/guntiso/mojoz)
Commands are provided such as `fetch` - gets data from server, `save` - saves data to server,
`delete` - deletes data from server, `set` - sets model data from client side.

Client has deferred request support, i.e. server returns request identifier and through web sockets
informs about progress.

Service data are form [`JsonModel.FormModel`](JsonModel#FormModel) or
list [`JsonModel.ListModel`](JsonModel#ListModel) based.

# Types
@docs JsonValue, JsonFormModel, JsonFormMsg, JsonListModel,
      JsonListMsg, Decoder, Encoder, FormModel,
      FormMsg, ListModel, ListMsg, Model, Msg, Path,
      SearchParams, Tomsg

# Initialization, configuration
@docs initJsonList, initList, initJsonForm, initForm, listDecoder, formDecoder, countBaseUri,
      pageSize, countDecoder, idParam, offsetLimitParams,
      toDeferredMsg, deferredSettings, defaultDeferredSettings

# Commands
@docs fetch, fetchFromStart, fetchDeferred, fetchDeferredFromStart, fetchCount, fetchCountDeferred,
      fetchMetadata, set, edit, save, create, delete

# Data examination
@docs data, progress, isProgress, completed, count, isEmpty, id, searchPars

# Metadata examination
@docs conf, columnNames, visibleColumnNames, fieldNames, visibleFieldNames

# Utility functions
@docs isInitialized, notInitialized, ready, jsonDataDecoder, pathDecoder

# Model update
@docs update
-}


import Json.Decode as JD
import Json.Encode as JE
import Dict exposing (Dict)
import Set exposing (Set)
import Http
import Task

import ViewMetadata as VM
import DeferredRequests as DR
import Ask
import Utils

import Debug exposing (log, toString)


type alias TypeName = String


{-| http query parameters as list of tuples. -}
type alias SearchParams = List (String, String)


{-| Decoder based on metadata provided by first parameter, view name provided by second. -}
type alias Decoder value = Dict String VM.View -> String -> JD.Decoder value


{-| Encoder based on metadata provided by first parameter, view name provided by second. -}
type alias Encoder value = Dict String VM.View -> String -> value -> JD.Value


type alias Setter msg value = value -> Model msg value -> Model msg value


type alias JsonEditor value = Path -> JsonValue -> value -> value


type alias JsonReader value = Path -> value -> Maybe JsonValue


type alias JsonTraverser a =
  { before: String -> JsonValue -> a -> a
  , on: String -> VM.Field -> JsonValue -> a -> a
  , after: String -> JsonValue -> a -> a
  }


type alias DeferredHeader = (String, String)


{-| Function which generates DataMsg command -}
type alias DataFetcher msg value =
  Tomsg msg value -> Bool -> SearchParams -> Maybe DeferredHeader -> Model msg value -> Cmd msg


{-| Function which generates CountMsg command -}
type alias CountFetcher msg value =
  Tomsg msg value -> SearchParams -> Maybe DeferredHeader -> Model msg value -> Cmd msg


type alias DeferredConfig =
  { timeout: Maybe String
  }


type alias Progress =
  { fetchProgress: Bool
  , countProgress: Bool
  , metadataProgress: Bool
  }


type alias Data value =
  { data: value
  , count: Maybe Int
  , completed: Bool
  , searchParams: SearchParams
  , progress: Progress
  , ready: Bool
  }


type alias Config msg value =
  { typeName: String
  , decoder: Decoder value
  , encoder: Encoder value
  , setter: Setter msg value
  , editor: JsonEditor value
  , reader: JsonReader value
  , emptyData: Data value
  , metadataBaseUri: String
  , dataBaseUri: String
  , uri: Bool -> SearchParams -> Model msg value -> String
  , saveUri: SearchParams -> Model msg value -> String
  , createUri: SearchParams -> Model msg value -> String
  , metadata: Dict String VM.View
  , fieldGetter: VM.View -> List VM.Field
  , metadataFetcher: String -> Cmd VM.Msg
  , dataFetcher: DataFetcher msg value
  , countFetcher: CountFetcher msg value
  , deferredConfig: Maybe DeferredConfig
  , countBaseUri: String
  , countUri: SearchParams -> Model msg value -> String
  , countDecoder: JD.Decoder Int
  , pageSize: Int
  , idParamName: String
  , id: Model msg value -> Maybe String
  , offsetParamName: String
  , limitParamName: String
  , toMessagemsg: Ask.Tomsg msg
  , queuedCmd: Maybe (Msg msg value)
  }


{-| Json model. Consists of data and configuration. -}
type Model msg value = Model (Data value) (Config msg value)


-- list types

{-| Json model based on list values. -}
type alias ListModel msg value = Model msg (List value)


{-| Json model based on `JsonValue` list. This model comes with encoder, decoder and
can be partialy edited. See [`edit`](#edit)
-}
type alias JsonListModel msg = ListModel msg JsonValue


{-| Message for [`ListModel`](#ListModel) -}
type alias ListMsg msg value = Msg msg (List value)


{-| Message for [`JsonListModel`](#JsonListModel) -}
type alias JsonListMsg msg = ListMsg msg JsonValue


--form types
{-| Json model based on abstract value -}
type alias FormModel msg value = Model msg value


{-| Json model based on `JsonValue`. This comes with encoder, decoder and can be partialy
edited. See [`edit`](#edit)
-}
type alias JsonFormModel msg = FormModel msg JsonValue


{-| Message for [`FormModel`](#FormModel) -}
type alias FormMsg msg value = Msg msg value


{-| Message for [`JsonFormModel`](#JsonFormModel) -}
type alias JsonFormMsg msg = FormMsg msg JsonValue


{-| Data for dynamic Json models.
-}
type JsonValue
  = JsString String
  | JsNumber Float
  | JsBool Bool
  | JsNull
  | JsList (List JsonValue)
  | JsObject (Dict String JsonValue)


{-| Path to field in dynamic model data.
-}
type Path
  = Name String Path
  | Idx Int Path
  | End


{-| Message for model update. -}
type Msg msg value
  = MetadataMsg (Maybe (Cmd msg)) VM.Msg
  | DataMsg TypeName Bool SearchParams (Result Http.Error value)
  | CountMsg TypeName SearchParams (Result Http.Error Int)
  | DeleteMsg TypeName SearchParams (Result Http.Error String)
  | EditMsg Path JsonValue
  | MetadataMsgCmd (Maybe (Cmd msg))
  | UpdateCmdMsg Bool value
  | DataCmdMsg Bool Bool SearchParams (Maybe DeferredHeader)
  | CountCmdMsg Bool SearchParams (Maybe DeferredHeader)
  | SaveCmdMsg Bool SearchParams
  | CreateCmdMsg Bool SearchParams
  | DeleteCmdMsg Bool SearchParams
  | DoneMsg Progress
  | DeferredSubscriptionMsg (DR.Tomsg msg -> Cmd msg) (DR.Tomsg msg)
  | DeferredResponseMsg Progress Http.Error Bool


{-| Json model message constructor -}
type alias Tomsg msg value = Msg msg value -> msg


-- initialization & configruation

{-| Initialize [`JsonValue`](JsonModel.JsonValue) based list model.

      initJsonList "/metadata" "/data" "my-view" AskMsg
-}
initJsonList: String -> String -> String -> Ask.Tomsg msg -> JsonListModel msg
initJsonList metadataBaseUri dataBaseUri typeName toMessagemsg =
  let
    decoder metadata deTypeName = JD.list <| jsonDataDecoder .fields metadata deTypeName

    encoder metadata eTypeName value =
      value |>
      JE.list (jsonDataEncoder .fields metadata eTypeName)

    editor path value edata =
      let
        result =
          case jsonEditor path value <| JsList edata of
            JsList rows -> rows

            _ -> edata
      in
        case path of
          End -> result

          Idx _ _ -> result

          Name _ _ -> edata -- list editing path cannot start with name

    reader path value =
      case path of
        End -> Just <| JsList value

        Idx _ _ -> jsonReader path <| JsList value

        Name _ _ -> Nothing -- list reading cannot start with name

    jsonModel (Model md mc) =
      Model md
        { mc |
          decoder = decoder
        , encoder = encoder
        , editor = editor
        , reader = reader
        }
  in
    jsonModel <|
      initList
        metadataBaseUri
        dataBaseUri
        typeName
        (jsonDataDecoder .fields Dict.empty "")
        (jsonDataEncoder .fields Dict.empty "")
        toMessagemsg


{-| Initialize `value` based list model.

      initList "/metadata" "/data" "my-view" decoder encoder AskMsg
-}
initList: String -> String -> String -> JD.Decoder value -> (value -> JD.Value) -> Ask.Tomsg msg -> Model msg (List value)
initList metadataBaseUri dataBaseUri typeName decoder encoder toMessagemsg =
  let
    emptyListData =
      Data
        []
        Nothing
        False
        []
        (Progress False False False)

    listSetter newData (Model md mc) =
      Model
        { md |
          data = md.data ++ newData
        , count = Nothing
        , completed = List.length newData < mc.pageSize
        , ready = True
        }
        mc

    listUri restart searchParams (Model md mc) =
      let
        offset =
          if not restart && searchParams == md.searchParams then
            List.length md.data
          else 0

        searchParamsWithOffsetLimit =
          List.append
            searchParams
            [ (mc.offsetParamName, String.fromInt <| offset)
            , (mc.limitParamName, String.fromInt mc.pageSize)
            ]

        queryString = Utils.httpQuery searchParamsWithOffsetLimit
      in
        mc.dataBaseUri ++ "/" ++ mc.typeName ++ queryString

    countUri searchParams (Model _ mc) =
      let
        queryString = Utils.httpQuery searchParams
      in
        mc.countBaseUri ++ "/" ++ mc.typeName ++ queryString
  in
    Model
      (emptyListData False) -- model not ready upon initialization
      { typeName = typeName
      , decoder = \_ _ -> JD.list decoder
      , encoder = \_ _ value -> JE.list encoder value
      , setter = listSetter
      , editor = \_ _ value -> value
      , reader = \_ _ -> Nothing
      , emptyData = emptyListData True -- model ready when emptyData function called
      , metadataBaseUri = metadataBaseUri
      , dataBaseUri = dataBaseUri
      , uri = listUri
      , saveUri = \_ _ -> ""
      , createUri = \_ _ -> ""
      , metadata = Dict.empty
      , fieldGetter = .fields
      , metadataFetcher = VM.fetchMetadata
      , dataFetcher = httpDataFetcher
      , countFetcher = httpCountFetcher
      , deferredConfig = Nothing
      , countBaseUri = ""
      , countDecoder = JD.int
      , countUri = countUri
      , pageSize = 25
      , idParamName = "id"
      , id = always Nothing
      , offsetParamName = "offset"
      , limitParamName = "limit"
      , toMessagemsg = toMessagemsg
      , queuedCmd = Nothing
      }


{-| Initialize [`JsonValue`](JsonModel.JsonValue) based form model.

      initJsonForm "/metadata" "/data" "my-view" AskMsg
-}
initJsonForm: String -> String -> String -> Ask.Tomsg msg -> JsonFormModel msg
initJsonForm =
  initJsonValueForm .fields


{-| Initialize [`JsonValue`](JsonModel.JsonValue) based query form model.

      initJsonForm "/metadata" "/data" "my-view" AskMsg
-}
initJsonQueryForm: String -> String -> String -> Ask.Tomsg msg -> JsonFormModel msg
initJsonQueryForm =
  initJsonValueForm .filter


initJsonValueForm: (VM.View -> List VM.Field) -> String -> String -> String -> Ask.Tomsg msg -> JsonFormModel msg
initJsonValueForm fieldGetter metadataBaseUri dataBaseUri typeName toMessagemsg =
  let
    decoder metadata deTypeName = jsonDataDecoder fieldGetter metadata deTypeName

    encoder metadata eTypeName value = jsonDataEncoder fieldGetter metadata eTypeName value

    editor path value edata =
      let
        result =
          case jsonEditor path value edata of
            JsObject fields -> JsObject fields

            _ -> edata
      in
        case path of
          End -> result

          Name _ _ -> result

          Idx _ _ -> edata -- form editing path cannot start with index

    reader path value =
      case path of
        End -> Just value

        Name _ _ -> jsonReader path value

        Idx _ _ -> Nothing -- form reading cannot start with index

    formId (Model md mc) =
      mc.reader (Name mc.idParamName End) md.data |>
      Maybe.map jsonValueToString

    jsonModel (Model md mc) =
      Model md
       { mc |
         decoder = decoder
       , encoder = encoder
       , editor = editor
       , reader = reader
       }
  in
    jsonModel <|
      initFormInternal
        fieldGetter
        metadataBaseUri
        dataBaseUri
        typeName
        (jsonDataDecoder fieldGetter Dict.empty "")
        (jsonDataEncoder fieldGetter Dict.empty "")
        (JsObject Dict.empty)
        formId
        toMessagemsg


{-| Initialize `value` based form model on metadata fields.
      initForm "/metadata" "/data" "my-view" decoder encoder initValue idFunction AskMsg
-}
initForm:
  String -> String -> String ->
  JD.Decoder value -> (value -> JD.Value) ->
  value -> (FormModel msg value -> Maybe String) ->
  Ask.Tomsg msg ->
  FormModel msg value
initForm =
  initFormInternal .fields

{-| Initialize `value` based form model on metadata filter fields.
      initForm "/metadata" "/data" "my-view" decoder encoder initValue idFunction AskMsg
-}
initQueryForm:
  String -> String -> String ->
  JD.Decoder value -> (value -> JD.Value) ->
  value -> (FormModel msg value -> Maybe String) ->
  Ask.Tomsg msg ->
  FormModel msg value
initQueryForm =
  initFormInternal .filter


initFormInternal:
  (VM.View -> List VM.Field) ->
  String -> String -> String ->
  JD.Decoder value -> (value -> JD.Value) ->
  value -> (FormModel msg value -> Maybe String) ->
  Ask.Tomsg msg ->
  FormModel msg value
initFormInternal fieldGetter metadataBaseUri dataBaseUri typeName decoder encoder initValue formId toMessagemsg =
  let
    setter newdata (Model md mc) =
      Model
        { md |
          data = newdata
        , count = Nothing
        , completed = True
        , ready = True
        }
        mc

    query = Utils.httpQuery

    maybeIdPathAndQueryString idParamName params =
      (case List.partition (\(n, _) -> n == idParamName) params of
        ([], pars) -> query pars

        ((_, fid) :: tail, pars) -> "/" ++ fid ++ query pars
      )

    formUri _ searchParams (Model _ mc) =
      mc.dataBaseUri ++ "/" ++ mc.typeName ++
        (maybeIdPathAndQueryString mc.idParamName searchParams)

    saveUri searchParams ((Model _ mc) as model) =
      let
        uriEnd =
            id model |>
            Maybe.map (Tuple.pair mc.idParamName) |>
            Maybe.map (Utils.flip (::) searchParams) |>
            Maybe.map (maybeIdPathAndQueryString mc.idParamName) |>
            Maybe.withDefault (maybeIdPathAndQueryString mc.idParamName searchParams)
      in
        mc.dataBaseUri ++ "/" ++ mc.typeName ++ uriEnd

    createUri searchParams (Model _ mc) =
      mc.dataBaseUri ++ "/create/" ++ mc.typeName ++ query searchParams

    emptyFormData =
      Data
        initValue
        Nothing
        False
        []
        (Progress False False False)
  in
    Model
      (emptyFormData False) -- model not ready upon initialization
      { typeName = typeName
      , decoder = \_ _ -> decoder
      , encoder = \_ _ value -> encoder value
      , setter = setter
      , editor = \_ _ value -> value -- not implemented (only for Json... model)
      , reader = \_ _ -> Nothing -- not implemented (only for Json... model)
      , emptyData = emptyFormData True -- model ready when emptyData function called
      , metadataBaseUri = metadataBaseUri
      , dataBaseUri = dataBaseUri
      , uri = formUri
      , saveUri = saveUri
      , createUri = createUri
      , metadata = Dict.empty
      , fieldGetter = fieldGetter
      , metadataFetcher = VM.fetchMetadata
      , dataFetcher = httpDataFetcher
      , countFetcher = httpCountFetcher
      , deferredConfig = Nothing
      , countBaseUri = ""
      , countDecoder = JD.int
      , countUri = (\_ _ -> "")
      , pageSize = 0
      , idParamName = "id"
      , id = formId
      , offsetParamName = "offset"
      , limitParamName = "limit"
      , toMessagemsg = toMessagemsg
      , queuedCmd = Nothing
      }


{-| Set list model decoder
-}
listDecoder: Decoder (List value) -> ListModel msg value -> ListModel msg value
listDecoder decoder (Model d c) = Model d { c | decoder = decoder }


{-| Set form model decoder
-}
formDecoder: Decoder value -> FormModel msg value -> FormModel msg value
formDecoder decoder (Model d c) = Model d { c | decoder = decoder }


{-| Set count base uri
-}
countBaseUri: String -> Model msg value -> Model msg value
countBaseUri uri (Model d c) = Model d { c | countBaseUri = uri }


{-| Set page size for list model - [`JsonModel.ListModel`](JsonModel#ListModel)
-}
pageSize: Int -> Model msg value -> Model msg value
pageSize ps (Model d c) = Model d { c | pageSize = ps }


{-| Set count decoder for list model - [`JsonModel.ListModel`](JsonModel#ListModel)
-}
countDecoder: JD.Decoder Int -> Model msg value -> Model msg value
countDecoder decoder (Model d c) = Model d { c | countDecoder = decoder }


{-| Set id parameter name for form model - [`JsonModel.FormModel`](JsonModel#FormModel)
-}
idParam: String -> Model msg value -> Model msg value
idParam param (Model d c) = Model d { c | idParamName = param }


{-| Set offset, limit parameter names for list model - [`JsonModel.ListModel`](JsonModel#ListModel)
-}
offsetLimitParams: String -> String -> Model msg value -> Model msg value
offsetLimitParams offset limit (Model d c) =
  Model d { c | offsetParamName = offset, limitParamName = limit }


{-| Configure deferred requests [`DeferredRequests`](DeferredRequests)

    enableDeferred (Just "180s") model
-}
enableDeferred: Model msg value -> Model msg value
enableDeferred (Model d c) =
  Model d { c | deferredConfig = Just <| DeferredConfig Nothing }


{-| Configure deferred requests [`DeferredRequests`](DeferredRequests)

    enableDeferred (Just "180s") model
-}
enableDeferredWithTimeout: String -> Model msg value -> Model msg value
enableDeferredWithTimeout timeout (Model d c) =
  Model d { c | deferredConfig = Just <| DeferredConfig <| Just timeout }


{-| Set data fetcher
-}
dataFetcher: DataFetcher msg value -> Model msg value -> Model msg value
dataFetcher fetcher (Model d c) =
  Model d { c | dataFetcher = fetcher }


{-| Set count fetcher
-}
countFetcher: CountFetcher msg value -> Model msg value -> Model msg value
countFetcher fetcher (Model d c) =
  Model d { c | countFetcher = fetcher }


{-| Set data directly into model. This will not force metadata initialization as
    if when [`set`](#set) command is called. Other fields like [`count`] are not
    modified.
-}
setData: value -> Model msg value -> Model msg value
setData value (Model d c) =
  Model { d | data = value } c

-- Data getter functions

{-| Get model data.
-}
data: Model msg value -> value
data (Model d _) = d.data


{-| Check model [`Progress`](#Progress)
-}
progress: Model msg value -> Progress
progress (Model d _) = d.progress


{-| Check whether on if [`Progress`](#Progress) flags is set.
-}
isProgress: Model msg value -> Bool
isProgress (Model d _ ) =
  (\ { fetchProgress, countProgress, metadataProgress } ->
    fetchProgress || countProgress || metadataProgress
  ) d.progress


{-| Check whether all data are fetched from server. Relevant for [`ListModel`](#ListModel)
    since data are fetched in pages.
-}
completed: Model msg value -> Bool
completed (Model d _) = d.completed


{-| Maybe returns count of potential [`ListModel`](#ListModel)
    data. **Note** value is not dependant of actually fetched data in model.
-}
count: Model msg value -> Maybe Int
count (Model d _) = d.count


{-| Checks whether [`ListModel`](#ListModel) is empty
-}
isEmpty: ListModel msg value -> Bool
isEmpty model = List.isEmpty <| data model


{-| Maybe returns id string of model data
-}
id: Model msg value -> Maybe String
id ((Model _ c) as model) =
  c.id model


{-| Return search params -}
searchPars: Model msg value -> SearchParams
searchPars (Model { searchParams } _) = searchParams


{-| True if metadata are not empty and metadata fetch is not progress
-}
isInitialized: Model msg value -> Bool
isInitialized model = not (notInitialized model)


{-| True if metadata are empty or metadata fetch is in progress
-}
notInitialized: Model msg value -> Bool
notInitialized (Model d { metadata }) =
  Dict.isEmpty metadata || d.progress.metadataProgress


{-| True if data are set at least once during Model lifetime
-}
ready: Model msg value -> Bool
ready (Model d _) =
  d.ready

-- metadata examination

{-| Returns column names from metadata
-}
columnNames: Model msg value -> List String
columnNames (Model _ c as model) = fieldNames True c.typeName model


{-| Returns visible column names from metdata
-}
visibleColumnNames: Model msg value -> List String
visibleColumnNames (Model _ c as model) = visibleFieldNames c.typeName model


{-| Returns visible field names from metadata indicated by string parameter
(child structure may be specified)
-}
visibleFieldNames: String -> Model msg value -> List String
visibleFieldNames typeName model =
  fieldNames False typeName model


{-| If bool parameter is True return all field labels else only visible.
String parameter may indicated child structure.
-}
fieldNames: Bool -> String -> Model msg value -> List String
fieldNames all typeName model =
  mdStringValue all typeName .name model


{-| Returns column labels from metadata
-}
columnLabels: Model msg value -> List String
columnLabels (Model _ c as model) = fieldLabels True c.typeName model


{-| Returns visible column labels from metdata
-}
visibleColumnLabels: Model msg value -> List String
visibleColumnLabels (Model _ c as model) = visibleFieldLabels c.typeName model


{-| Returns visible field labels from metadata indicated by string parameter
(child structure may be specified)
-}
visibleFieldLabels: String -> Model msg value -> List String
visibleFieldLabels typeName model =
  fieldLabels False typeName model


{-| If bool parameter is True return all field labels else only visible.
String parameter may indicated child structure.
-}
fieldLabels: Bool -> String -> Model msg value -> List String
fieldLabels all typeName model =
  mdStringValue all typeName .label model


{-| Returns metadata field by name from main view -}
field: String -> Model msg value -> Maybe VM.Field
field fieldName (Model _ { metadata, typeName, fieldGetter }) =
  metadata |>
  Dict.get typeName |>
  Maybe.map fieldGetter |>
  Maybe.andThen (Utils.find (.name >> (==) fieldName))


mdStringValue: Bool -> String -> (VM.Field -> String) -> Model msg value -> List String
mdStringValue all typeName valFun (Model _ { metadata, fieldGetter }) =
  Dict.get typeName metadata |>
  Maybe.map fieldGetter |>
  Maybe.map (List.filter (.visible >> (||) all)) |>
  Maybe.map (List.map valFun) |>
  Maybe.withDefault []


{-| Returns model configuration
-}
conf: Model msg value -> Config msg value
conf (Model _ c) = c

-- utility functions


{-| Decoder for [`JsonValue`](#JsonValue)

    jsonDataDecoder metadata viewTypeName
-}
jsonDataDecoder: (VM.View -> List VM.Field) -> Dict String VM.View -> String -> JD.Decoder JsonValue
jsonDataDecoder fieldGetter metadata viewTypeName =
  let
    fail name = JD.fail <| "Metadata not found for type: " ++ name

    primDec jsonType = case jsonType of
      "string" ->
        JD.oneOf [ JD.string |> JD.map JsString, JD.null JsNull ]

      "number" ->
        JD.oneOf [ JD.float |> JD.map JsNumber, JD.null JsNull ]

      "boolean" ->
        JD.oneOf [ JD.bool |> JD.map JsBool, JD.null JsNull ]

      x ->
        JD.fail ("<unknown json type: " ++ x ++ " >")

    fieldDecoder fieldmd =
      if (not fieldmd.isComplexType) && (not fieldmd.isCollection) then
        primDec fieldmd.jsonType
      else if fieldmd.isComplexType && (not fieldmd.isCollection) then
        Dict.get fieldmd.typeName metadata |>
        Maybe.map objectDecoder |>
        Maybe.withDefault (fail fieldmd.typeName)
      else if (not fieldmd.isComplexType) && fieldmd.isCollection then
        JD.list (primDec fieldmd.jsonType) |>
        JD.map JsList
      else
        Dict.get fieldmd.typeName metadata |>
        Maybe.map list_decoder |>
        Maybe.map (\ld -> JD.map JsList ld) |>
        Maybe.withDefault (fail fieldmd.typeName)

    objectDecoder viewmd =
      let
        fieldsDecoder fields decodedFields =
          case fields of
            [] -> JD.succeed <| List.reverse decodedFields

            fieldmd :: tail ->
              JD.oneOf
                [ JD.field fieldmd.name (JD.lazy (\_ -> fieldDecoder fieldmd)) |> -- use lazy decoder to avoid possible metadata recurcion
                  JD.andThen (\df -> fieldsDecoder tail <| (fieldmd.name, df) :: decodedFields)
                , fieldsDecoder tail decodedFields -- field not found or failed to decode
                ]
      in
        JD.oneOf [ fieldsDecoder (fieldGetter viewmd) [], JD.null [] ] |>
        JD.map Dict.fromList |>
        JD.map JsObject

    list_decoder viewmd = JD.oneOf [ JD.list (objectDecoder viewmd), JD.null [] ]
  in
    Dict.get viewTypeName metadata |>
    Maybe.map objectDecoder |>
    Maybe.withDefault (fail viewTypeName)


{-| Encoder for [`JsonValue`](#JsonValue)

    jsonDataEncoder metadata viewTypeName value
-}
jsonDataEncoder: (VM.View -> List VM.Field) -> Dict String VM.View -> String -> JsonValue -> JD.Value
jsonDataEncoder fieldGetter metadata viewTypeName value =
  let
    encodePrimField fv = case fv of
      JsString v -> JE.string v

      JsNumber v -> JE.float v

      JsBool v -> JE.bool v

      JsNull -> JE.null

      _ -> JE.null -- unexpected element, encode as null

    encodeObject rv vmd = case rv of
      JsObject fields ->
        fieldGetter vmd |>
        List.concatMap
          (\fmd ->
            Dict.get fmd.name fields |>
            Maybe.map
              (\fv ->
                ( fmd.name
                , if (not fmd.isComplexType) && (not fmd.isCollection) then
                    encodePrimField fv
                  else if fmd.isComplexType && (not fmd.isCollection) then
                    Dict.get fmd.typeName metadata |>
                    Maybe.map (encodeObject fv) |>
                    Maybe.withDefault JE.null
                  else if (not fmd.isComplexType) && fmd.isCollection then
                    case fv of
                      JsList primitiveValues ->
                        primitiveValues |>
                        JE.list encodePrimField

                      _ -> JE.list identity []
                  else
                    Dict.get fmd.typeName metadata |>
                    Maybe.map (encodeList fv) |>
                    Maybe.withDefault JE.null
                ) :: []
              ) |>
            Maybe.withDefault []
          ) |>
        JE.object

      _ -> JE.null -- unexpected element, encode as null

    encodeList lv vmd = case lv of
      JsList values ->
        values |>
        JE.list ((Utils.flip encodeObject) vmd)

      _ -> JE.null -- unexpected element, encode as null
  in
    Dict.get viewTypeName metadata |>
    Maybe.map (encodeObject value) |>
    Maybe.withDefault JE.null


jsonValueToString: JsonValue -> String
jsonValueToString jsValue =
  case jsValue of
    JsString v -> v

    JsNumber v -> String.fromFloat v

    JsBool v -> toString v

    JsNull -> ""

    x -> toString x


stringToJsonValue: String -> String -> Maybe JsonValue
stringToJsonValue jsonType value =
  case jsonType of
    "string" ->
      Just <| JsString value

    "number" ->
      String.toFloat value |> Maybe.map JsNumber

    "boolean" ->
      Just <| JsBool (String.toLower value |> (==) "true")

    x ->
      Just JsNull


traverseJson: Dict String VM.View -> JsonTraverser a -> String -> JsonValue -> a -> a
traverseJson metadata ({ before, on, after } as traverser) typeName value result =
  let
    trj = traverseJson metadata traverser
  in
    Dict.get typeName metadata |>
    Maybe.map
      (\view ->
        case value of
          JsObject fields ->
            List.foldl
              (\fmd res ->
                Dict.get fmd.name fields |>
                Maybe.map
                  (\val ->
                    if fmd.isComplexType then
                      trj
                        fmd.typeName
                        val
                        res
                    else
                      on view.typeName fmd val res
                  ) |>
                Maybe.withDefault res
              )
              (before view.typeName value result)
              view.fields |>
            (\res -> after view.typeName value res)

          JsList vals ->
            List.foldl
              (\v r -> trj typeName v r)
              (before view.typeName value result)
              vals |>
            (\res -> after view.typeName value res)

          _ -> result -- structure not according to metadata
      ) |>
    Maybe.withDefault result


jsonList: JsonListModel msg -> List (List String)
jsonList (Model _ {typeName, metadata} as m) =
  let
    decRow vmd row =
      vmd.fields |>
      List.concatMap
        (\f ->
          Dict.get f.name row |>
          Maybe.map
            (\fv ->
              case fv of
                JsList rows ->
                  if not f.isComplexType then
                    rows |>
                    List.map (\rjv -> jsonValueToString rjv) |>
                    List.intersperse ", " |>
                    String.concat
                  else
                    toString rows

                x -> jsonValueToString x
            ) |>
          Maybe.map List.singleton |>
          Maybe.withDefault []
        )

    jlist d md =
      d |>
      List.map
        (\r -> case r of
          JsObject fields -> --fields
            decRow md fields

          x -> [ jsonValueToString x ] --should not happen
        )
  in
    Dict.get typeName metadata |>
    Maybe.map (jlist (data m)) |>
    Maybe.withDefault []


flattenJsonForm: (VM.View -> List VM.Field) -> JsonFormModel msg -> List (Path, VM.Field, JsonValue)
flattenJsonForm fieldGetter (Model _ { typeName, metadata } as m) =
  let
    flatten viewmd path jsonData result =
      case jsonData of
        JsObject values ->
          fieldGetter viewmd |>
          List.foldl
            (\f res ->
              Dict.get f.name values |>
              Maybe.map
                (\v ->
                  let
                    fpath = Name f.name path
                  in
                    if f.isComplexType then
                      Dict.get f.typeName metadata |>
                      Maybe.map
                        (\vmd ->
                          if f.isCollection then
                            case v of
                              JsList rows ->
                                List.foldl
                                  (\fv (nres, i) -> (flatten vmd (Idx i fpath) fv nres, i + 1))
                                  (res, 0)
                                  rows |>
                                Tuple.first

                              _ -> res --unexpected match, structure not according to metadata
                          else
                            flatten vmd fpath v res
                        ) |>
                      Maybe.withDefault res
                    else
                      if f.isCollection then
                        case v of
                          JsList rows ->
                            List.foldl
                              (\fv (nres, i) -> ((Idx i fpath, f, fv) :: nres, i + 1))
                              (res, 0)
                              rows |>
                            Tuple.first

                          _ -> res --unexpected match, structure not according to metadata
                      else
                        (fpath, f, v) :: res
                ) |>
              Maybe.withDefault ((Name f.name path, f, JsNull) :: res) -- set value to JsNull if field no present
            )
            result

        _ -> result -- unexpected match, structure not according to metadata
  in
    Dict.get typeName metadata |>
    Maybe.map(\md -> flatten md End (data m) []) |>
    Maybe.map (List.foldl (\(p, f, v) nres -> (reversePath p, f, v) :: nres) []) |>
    Maybe.withDefault []


searchParsFromJson: JsonFormModel msg -> SearchParams
searchParsFromJson (Model _ { metadata, typeName } as m) =
  let
    par fld val res =
      case val of
        JsList vals ->
          List.foldl (par fld) res vals

        JsObject vals ->
          pars res <| JsObject vals

        x ->
          jsonValueToString val |>
          (\v ->
            if String.isEmpty v then res
            else (fld, jsonValueToString val) :: res
          )

    pars res vals =
      case vals of
        JsObject jsons ->
          Dict.foldl (\f v r -> par f v r) res jsons

        _ ->
          res
  in
    pars [] <| data m


{-| Decoder of json array of strings and ints, i.e. ["department-name", 2, "employee-name"]
    or just "department-name" or just idx like - 1
-}
pathDecoder: JD.Decoder Path
pathDecoder =
  let
    nameDec = JD.string |> JD.map ((Utils.flip Name) End)

    idxDec = JD.int |> JD.map ((Utils.flip Idx) End)

    pathElementDecoder = JD.oneOf [ nameDec, idxDec ]

    pathDec =
      JD.list pathElementDecoder |>
      JD.map
        (List.foldr
          (\el path ->
            case el of
              Name n _ -> Name n path

              Idx i _ -> Idx i path

              End -> End
          )
          End
      )
  in
    JD.oneOf [ nameDec, pathDec, idxDec ]


pathEncoder: Path -> JD.Value
pathEncoder path =
  let
    encode res p =
      case p of
        Name n rest ->
          JE.string n :: encode res rest

        Idx i rest ->
          JE.int i :: encode res rest

        End -> res
  in case encode [] path of
    [ val ] -> val

    x -> JE.list identity x


reversePath: Path -> Path
reversePath path =
  let
    reverse np p =
      case p of
        End -> np

        Name n rest -> reverse (Name n np) rest

        Idx i rest as ie -> reverse (Idx i np) rest
  in
    reverse End path


-- commands

{-| Fetch view data.

    fetch toMsg [("id", "1")]
-}
fetch: Tomsg msg value -> SearchParams -> Cmd msg
fetch toMsg searchParams =
  Task.perform identity <| Task.succeed <| fetchMsg toMsg searchParams


fetchMsg: Tomsg msg value -> SearchParams -> msg
fetchMsg toMsg searchParams =
  toMsg <| DataCmdMsg True False searchParams Nothing


{-| Fetch view data from starting from first record. Relevant for [`ListModel`](#ListModel)

    fetch toMsg [("id", "1")]
-}
fetchFromStart: Tomsg msg value -> SearchParams -> Cmd msg
fetchFromStart toMsg searchParams =
  Task.perform identity <| Task.succeed <| fetchFromStartMsg toMsg searchParams


fetchFromStartMsg: Tomsg msg value -> SearchParams -> msg
fetchFromStartMsg toMsg searchParams =
  toMsg <| DataCmdMsg True True searchParams Nothing


{-| Fetch view data with deferred header set.
-}
fetchDeferred: Tomsg msg value -> SearchParams -> DeferredHeader -> Cmd msg
fetchDeferred toMsg searchParams deferredHeader =
  Task.perform toMsg <| Task.succeed <| DataCmdMsg True False searchParams <| Just deferredHeader


{-| Fetch view data with deferred header set starting from first record.
    Relevant for [`ListModel`](#ListModel)
-}
fetchDeferredFromStart: Tomsg msg value -> SearchParams -> DeferredHeader -> Cmd msg
fetchDeferredFromStart toMsg searchParams deferredHeader =
  Task.perform toMsg <| Task.succeed <| DataCmdMsg True True searchParams <| Just deferredHeader


{-| Low level function. Http data fetcher. This can be set as a dataFetcher function in model's configuration -}
httpDataFetcher: DataFetcher msg value
httpDataFetcher toMsg restart searchParams deferredHeader ((Model _ modelConf) as model) =
  Http.send
    (toMsg << DataMsg modelConf.typeName restart searchParams) <|
    dataHttpRequest (modelConf.uri restart searchParams model) deferredHeader <|
      modelConf.decoder modelConf.metadata modelConf.typeName


{-| Low level function. Http count fetcher. This can be set as a countFetcher function in model's configuration -}
httpCountFetcher: CountFetcher msg value
httpCountFetcher toMsg searchParams deferredHeader ((Model _ modelConf) as model) =
  Http.send
    (toMsg << CountMsg modelConf.typeName searchParams) <|
    dataHttpRequest (modelConf.countUri searchParams model) deferredHeader modelConf.countDecoder


{- Private method used in `httpDataFetcher`, `httpCountFetcher` -}
dataHttpRequest: String -> Maybe DeferredHeader -> JD.Decoder value -> Http.Request value
dataHttpRequest uri maybeHeader decoder =
  maybeHeader |>
    Maybe.map
      (\(header, value) ->
        Http.request
          { method = "GET"
          , headers =
              [ Http.header header value
              ]
          , url = uri
          , body = Http.emptyBody
          , expect = Http.expectJson decoder
          , timeout = Nothing
          , withCredentials = False
          }
      ) |>
    Maybe.withDefault (Http.get uri decoder)


{-| Fetches mojoz view field definition enum values -}
enumFetcher: String -> String -> (String -> String) -> DataFetcher msg (List String)
enumFetcher viewName fieldName mapper toMsg _ _ _ (Model _ modelConf) =
  let
    values =
      Dict.get viewName modelConf.metadata |>
      Maybe.andThen (VM.field fieldName) |>
      Maybe.andThen .enum |>
      Maybe.map (List.map mapper) |>
      Maybe.withDefault []
  in
    Task.perform (toMsg << DataMsg modelConf.typeName True []) <| Task.succeed <| Ok values


{-| Fetch record count. Command is available if [`countBaseUri`](#countBaseUri)
    is configured.
-}
fetchCount: Tomsg msg value -> SearchParams -> Cmd msg
fetchCount toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| CountCmdMsg True searchParams Nothing


{-| Fetch record count with deferred header set.
    Command is available if [`countBaseUri`](#countBaseUri) is configured.
-}
fetchCountDeferred: Tomsg msg value -> SearchParams -> DeferredHeader -> Cmd msg
fetchCountDeferred toMsg searchParams deferredHeader =
  Task.perform toMsg <| Task.succeed <| CountCmdMsg True searchParams <| Just deferredHeader


{-| Fetch view metadata.
-}
fetchMetadata: Tomsg msg value -> Cmd msg
fetchMetadata toMsg =
  Task.perform toMsg <| Task.succeed <| MetadataMsgCmd Nothing


{-| Set model value.
-}
set: Tomsg msg value -> value -> Cmd msg
set toMsg value =
  Task.perform toMsg <| Task.succeed <| UpdateCmdMsg True value


{-| Edit `JsonValue` model.
-}
edit: Tomsg msg value -> Path -> JsonValue -> Cmd msg
edit toMsg path value =
  Task.perform toMsg <| Task.succeed <| EditMsg path value


{-| Save model.
-}
save: Tomsg msg value -> SearchParams -> Cmd msg
save toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| SaveCmdMsg True searchParams


{-| Create model.
-}
create: Tomsg msg value -> SearchParams -> Cmd msg
create toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| CreateCmdMsg True searchParams


{-| Delete model.
-}
delete: Tomsg msg value -> SearchParams -> Cmd msg
delete toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| DeleteCmdMsg True searchParams


{-| Model updater. -}
update: Tomsg msg value -> Msg msg value -> Model msg value -> (Model msg value, Cmd msg)
update toMsg msg (Model modelData modelConf as same) =
  let
    --model construction
    maybeWithNewData restart searchParams (Model _ mc) =
      if not restart && searchParams == modelData.searchParams then
        same
      else let emptyData = mc.emptyData in
        Model { emptyData | searchParams = searchParams } mc
    withProgress pr (Model d c) = Model { d | progress = pr } c
    withEmptyQueue ((Model d c) as m) =
      if c.queuedCmd == Nothing then m else Model d { c | queuedCmd = Nothing }
    --progress
    fetchProgress = Progress True modelData.progress.countProgress False
    fetchDone = Progress False modelData.progress.countProgress False
    countProgress = Progress modelData.progress.fetchProgress True False
    countDone = Progress modelData.progress.fetchProgress False False
    -- metadata progress can be within other progress, so keep other values
    metadataProgress = modelData.progress |> (\p -> { p | metadataProgress = True })
    metadataDone = modelData.progress |> (\p -> { p | metadataProgress = False })
    allDone = Progress False False False
    isFetchProgress = modelData.progress.fetchProgress
    isCountProgress = modelData.progress.countProgress
    isMetadataProgress = modelData.progress.metadataProgress
    --message - model integrity check
    hasIntegrity (name, isPr) = name == modelConf.typeName && isPr
    unInitialized = notInitialized same -- shortcut function

    -- metadata fetch
    fetchMd andThen =
      Task.perform toMsg <| Task.succeed <| MetadataMsgCmd andThen

    initializeAndCmd noInitCmd cmd =
      if unInitialized then fetchMd <| Just <| noInitCmd () else cmd ()

    metadataHttpRequest maybeAndThen typeName =
      let
        mapper = MetadataMsg maybeAndThen >> toMsg
      in
        Cmd.map mapper <| modelConf.metadataFetcher (modelConf.metadataBaseUri ++ "/" ++ typeName)

    saveHttpRequest uri method value decoder =
      Http.request
        { method = method
        , headers = []
        , url = uri
        , body = Http.jsonBody value
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }

    deleteHttpRequest uri =
      Http.request
        { method = "DELETE"
        , headers = []
        , url = uri
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

    mapJsonHttpResult decoder jsonResult = -- used for deferred result decoding
      let
        emptyHttpResponse = Http.Response "" { code = 200, message = "" } Dict.empty ""
      in
        jsonResult |>
        Result.andThen
          (\jdata ->
            (Result.mapError
              ((Utils.flip Http.BadPayload) emptyHttpResponse)
              (Result.mapError JD.errorToString (JD.decodeValue decoder jdata))
            )
          )

    maybeSubscribeOrAskDeferred integrity subscription yes err progressDone =
      let
        processDeferredOrError maybeTimeout toDeferredmsg =
          DR.onHttpErrorCmd
            toDeferredmsg
            (toMsg << subscription)
            err
            (\question ((defhn, defhv) as defheader) ->
              let
                yescmd =
                  Task.perform (toMsg << yes) <| Task.succeed <|
                    ( maybeTimeout |>
                      Maybe.map (\t -> (defhn, t)) |>
                      Utils.orElse (Just defheader)
                    )

                nocmd =
                  Task.perform (toMsg << DoneMsg) <| Task.succeed progressDone
              in
                Ask.askmsg modelConf.toMessagemsg question yescmd <| Just nocmd
            )
            (toMsg << DeferredResponseMsg progressDone err)
      in
        if hasIntegrity integrity then
          modelConf.deferredConfig |>
          Maybe.map
            (\{ timeout } ->
              ( same
              , Ask.askToDeferredmsg
                  modelConf.toMessagemsg <|
                  toMsg << (DeferredSubscriptionMsg <| processDeferredOrError timeout)
              )
            ) |>
          Maybe.withDefault (errorResponse progressDone err)
        else
          errorResponse progressDone err

    queueCmd cmd =
      ( Model modelData { modelConf | queuedCmd = Just cmd }
      , Cmd.none
      )

    maybeUnqueueCmd (Model _ mc) =
      mc.queuedCmd |>
      Maybe.map (\qmsg -> Task.perform toMsg <| Task.succeed qmsg) |>
      Maybe.withDefault Cmd.none

    errorResponse pr err =
      log
        (toString err)
        ( same |> withProgress pr
        , Ask.errorOrUnauthorized modelConf.toMessagemsg err
        )
  in
    case msg of
      MetadataMsg maybeCmd (VM.ViewMetadataMsg (Ok view) children) ->
        if not isMetadataProgress then
          -- integrity violation metadata fetch must be in progress
          ( same, Cmd.none )
        else
          let
            newMetadata =
              Set.toList children |>
              List.map (\n -> (n, VM.View "" [] [])) |>
              Dict.fromList |>
              Dict.union (Dict.insert view.typeName view modelConf.metadata)
            newModel = Model modelData { modelConf | metadata = newMetadata }
            maybeNewMetadataCmd =
              Dict.filter (\_ v -> String.isEmpty v.typeName) newMetadata |>
              Dict.keys |>
              List.head |>
              Maybe.map (metadataHttpRequest maybeCmd)
          in
            maybeNewMetadataCmd |>
            Maybe.map (Tuple.pair newModel) |>
            Maybe.withDefault
              ( newModel |> withProgress metadataDone
              , maybeCmd |> Maybe.withDefault Cmd.none
              )

      DataMsg name restart searchParams (Ok newdata) ->
        let
          emptyQueueModel = withEmptyQueue same
        in
          ( if hasIntegrity (name, isFetchProgress) then
              let newModel = maybeWithNewData restart searchParams emptyQueueModel in
                modelConf.setter newdata newModel |> withProgress fetchDone
            else emptyQueueModel
          , maybeUnqueueCmd same
          )

      CountMsg name searchParams (Ok cnt) ->
        ( if hasIntegrity (name, modelData.progress.countProgress) then
            let
              newModel =
                  Model
                    { modelData |
                      -- if search params correspond set count else reset count
                      count = if (searchParams == modelData.searchParams) then Just cnt else Nothing
                    }
                    modelConf
            in
              newModel |> withProgress countDone |> withEmptyQueue
          else same |> withEmptyQueue
        , maybeUnqueueCmd same
        )

      DeleteMsg name searchParams (Ok newdata) ->
        let
          emptyQueueModel = withEmptyQueue same
        in
          ( if hasIntegrity (name, isFetchProgress) then
              emptyQueueModel |> (\(Model _ c) -> Model c.emptyData c)
            else emptyQueueModel
          , maybeUnqueueCmd same
          )

      DataMsg name restart searchParams (Err err) ->
        maybeSubscribeOrAskDeferred
          (name, isFetchProgress)
          ( DataMsg name restart searchParams <<
              mapJsonHttpResult
                (modelConf.decoder modelConf.metadata modelConf.typeName)
          )
          (DataCmdMsg False restart searchParams)
          err
          fetchDone

      CountMsg name searchParams (Err err) ->
        maybeSubscribeOrAskDeferred
          (name, isCountProgress)
          (CountMsg name searchParams <<
            mapJsonHttpResult modelConf.countDecoder
          )
          (CountCmdMsg False searchParams)
          err
          countDone

      DeleteMsg _ _ (Err err) -> errorResponse fetchDone err

      MetadataMsg _ (VM.ViewMetadataMsg (Err err) _) -> errorResponse allDone err

      EditMsg path value ->
        if unInitialized then
          ( same, let cmd = always <| edit toMsg path value in initializeAndCmd cmd cmd )
        else
          let
            good =
              hasIntegrity
                ( modelConf.typeName
                , not
                    ( isFetchProgress ||
                      isCountProgress ||
                      isMetadataProgress
                    )
                )
          in
            if good then
              let
                newValue = modelConf.editor path value modelData.data
              in
                ( Model { modelData | data = newValue } modelConf
                , Cmd.none
                )
            else ( same, Cmd.none )

      MetadataMsgCmd maybeAndThen ->
        if isMetadataProgress then
          ( same, Cmd.none )
        else
          ( (same |> withProgress metadataProgress)
          , metadataHttpRequest maybeAndThen modelConf.typeName
          )

      UpdateCmdMsg check value ->
        if check && isFetchProgress then
          queueCmd <| UpdateCmdMsg True value
        else
          let
            cmd =
              always <|
                Task.perform
                  (toMsg << DataMsg modelConf.typeName True modelData.searchParams)
                  (Task.succeed <| Ok value)

            noInitCmd =
              always <| Task.perform toMsg <| Task.succeed <| UpdateCmdMsg False value
          in
            ( (same |> withEmptyQueue |> withProgress fetchProgress), initializeAndCmd noInitCmd cmd )

      DataCmdMsg check restart searchParams deferredHeader ->
        if check && isFetchProgress then
          queueCmd <| DataCmdMsg True restart searchParams deferredHeader
        else
          let
            cmd =
              always <|
                modelConf.dataFetcher toMsg restart searchParams deferredHeader same

            noInitCmd =
              always <|
                Task.perform toMsg <|
                  Task.succeed <|
                    DataCmdMsg False restart searchParams deferredHeader

            newModel = same |> withEmptyQueue |> withProgress fetchProgress
          in
            ( newModel, initializeAndCmd noInitCmd cmd)

      CountCmdMsg check searchParams deferredHeader ->
        if String.isEmpty modelConf.countBaseUri then
          ( same, Ask.warn modelConf.toMessagemsg "Cannot calculate count, count uri empty" )
        else if check && isCountProgress then
          queueCmd <| CountCmdMsg True searchParams deferredHeader
        else
          let
            cmd =
              always <| modelConf.countFetcher toMsg searchParams deferredHeader same

            noInitCmd =
              always <|
                Task.perform toMsg <|
                  Task.succeed <|
                    CountCmdMsg False searchParams deferredHeader
          in

          ( same |> withEmptyQueue |> withProgress countProgress
          , initializeAndCmd noInitCmd cmd
          )

      SaveCmdMsg check searchParams ->
        if check && isFetchProgress then
          ( same, Ask.warn modelConf.toMessagemsg "Operation in progress, please try later." )
        else
          let
            method = id same |> Maybe.map (always "PUT") |> Maybe.withDefault "POST"

            value = modelConf.encoder modelConf.metadata modelConf.typeName modelData.data

            decoder = modelConf.decoder modelConf.metadata modelConf.typeName

            cmd =
              always <|
                Http.send
                  (toMsg << DataMsg modelConf.typeName False searchParams) <|
                  saveHttpRequest (modelConf.saveUri searchParams same) method value decoder

            noInitCmd =
              always <| Task.perform toMsg <| Task.succeed <| SaveCmdMsg False searchParams
          in
            ( (same |> withProgress fetchProgress), initializeAndCmd noInitCmd cmd )

      CreateCmdMsg check searchParams ->
        if check && isFetchProgress then
          ( same, Ask.warn modelConf.toMessagemsg "Operation in progress, please try later.")
        else
          let
            decoder = modelConf.decoder modelConf.metadata modelConf.typeName

            cmd =
              always <|
                Http.send
                  (toMsg << DataMsg modelConf.typeName False searchParams) <|
                  Http.get (modelConf.createUri searchParams same) decoder

            noInitCmd =
              always <| Task.perform toMsg <| Task.succeed <| CreateCmdMsg False searchParams
          in
            ( (same |> withProgress fetchProgress), initializeAndCmd noInitCmd cmd )

      DeleteCmdMsg check searchParams ->
        if check && isFetchProgress then
          ( same, Ask.warn modelConf.toMessagemsg "Operation in progress, please try later." )
        else
          let
            cmd =
              always <|
                Http.send
                  (toMsg << DeleteMsg modelConf.typeName searchParams) <|
                  deleteHttpRequest <| modelConf.saveUri searchParams same

            noInitCmd =
              always <| Task.perform toMsg <| Task.succeed <| DeleteCmdMsg False searchParams
          in
            ( (same |> withProgress fetchProgress), initializeAndCmd noInitCmd cmd )

      DoneMsg doneProgress ->
        ( same |> withProgress doneProgress, Cmd.none )

      DeferredSubscriptionMsg subscriptionCmd toDeferredmsg ->
        ( same
        , subscriptionCmd toDeferredmsg
        )

      DeferredResponseMsg doneProgressOnError err success ->
        if success then
          ( same, Cmd.none ) -- do nothing since result should arrive from deferred subscription or request with deferred header
        else
          errorResponse doneProgressOnError err


jsonEditor: Path -> JsonValue -> JsonValue -> JsonValue
jsonEditor path value model =
  let
    setRow rpath idx rows =
      ( rows |>
        List.indexedMap
          (\i val -> if i == idx then transform rpath val else val)
      )

    deleteRow rows idx =
      case
        List.foldl
          (\val (res, i) -> (if i == idx then res else val :: res, i + 1))
          ([], 0)
          rows
      of (l, _) -> List.reverse l

    insertRow row rows idx =
      case
        List.foldl
          (\val (res, i) -> (val :: (if i == idx then row :: res else res), i + 1))
          ([], 0)
          rows
      of (l, _) -> List.reverse l

    transform tpath tdata =
      case tpath of
        End -> value

        Name name rest ->
          case tdata of
            JsObject values ->
              JsObject <|
                Dict.update
                  name
                  (\mv ->
                    Just <|
                      transform rest (Maybe.withDefault JsNull mv)
                  )
                  values

            fv -> fv -- do nothing since element must match complex type

        Idx idx End ->
          case tdata of
            JsList rows ->
              JsList <|
                if idx < 0 then -- insert or delete row with index -idx - 1
                  case value of
                    JsNull -> deleteRow rows (-idx - 1) -- no data in value, delete row

                    _ -> insertRow value rows (-idx - 1)
                else setRow End idx rows

            fv -> fv -- do nothing since element must be record

        Idx idx rest ->
          case tdata of
            JsList rows -> JsList <| setRow rest idx rows

            fv -> fv -- do nothing since element must match complex type
  in
    transform path model


jsonEdit: String -> JsonValue -> JsonValue -> JsonValue
jsonEdit path value model =
  (if String.startsWith "[" path then path else String.concat ["\"", path, "\""]) |>
  JD.decodeString pathDecoder |>
  Result.toMaybe |>
  Maybe.map (\p -> jsonEditor p value model) |>
  Maybe.withDefault model


jsonReader: Path -> JsonValue -> Maybe JsonValue
jsonReader path value =
  case path of
    Name name rest ->
      case value of
        JsObject values ->
          Dict.get name values |>
          Maybe.andThen (jsonReader rest)

        _ -> Nothing -- cannot match name

    Idx idx rest ->
      case value of
        JsList values ->
          Utils.at idx values |>
          Maybe.andThen (\v -> jsonReader rest v)

        _ -> Nothing -- cannot match idx

    End ->
      Just value


jsonString: String -> JsonValue -> Maybe String
jsonString path source =
  (if String.startsWith "[" path then path else String.concat ["\"", path, "\""]) |>
  JD.decodeString pathDecoder |>
  Result.toMaybe |>
  Maybe.andThen (\p -> jsonReader p source) |>
  Maybe.andThen
    (\v -> case v of
      JsNumber n ->
        Just <| String.fromFloat n

      JsBool b ->
        Just <| toString b

      JsString s ->
        Just s

      x -> Just <| toString x
    )


jsonInt: String -> JsonValue -> Maybe Int
jsonInt path source =
  (if String.startsWith "[" path then path else String.concat ["\"", path, "\""]) |>
  JD.decodeString pathDecoder |>
  Result.toMaybe |>
  Maybe.andThen (\p -> jsonReader p source) |>
  Maybe.andThen
    (\v -> case v of
      JsNumber n ->
        Just <| round n

      _ -> Nothing
    )


jsonFloat: String -> JsonValue -> Maybe Float
jsonFloat path source =
  (if String.startsWith "[" path then path else String.concat ["\"", path, "\""]) |>
  JD.decodeString pathDecoder |>
  Result.toMaybe |>
  Maybe.andThen (\p -> jsonReader p source) |>
  Maybe.andThen
    (\v -> case v of
      JsNumber n ->
        Just n

      _ -> Nothing
    )


jsonBool: String -> JsonValue -> Maybe Bool
jsonBool path source =
  (if String.startsWith "[" path then path else String.concat ["\"", path, "\""]) |>
  JD.decodeString pathDecoder |>
  Result.toMaybe |>
  Maybe.andThen (\p -> jsonReader p source) |>
  Maybe.andThen
    (\v -> case v of
      JsBool b ->
        Just b

      _ -> Nothing
    )
