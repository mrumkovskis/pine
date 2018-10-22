module JsonModel exposing
  ( -- types
    Model (..), DataValue (..), Msg, ListModel, ListMsg, DataValueListModel
  , DataValueListMsg, FormModel, FormMsg, DataValueFormModel, DataValueFormMsg
  , Path (..), SearchParams, DeferredConfig, TimeoutDeferredConfig
  , Decoder, Encoder, Setter, Editor, Reader
  -- initialization, configuration
  , initDataValueList, initList, initDataValueForm, initForm, listDecoder, formDecoder
  , countBaseUri, pageSize, countDecoder, idParam, offsetLimitParams
  , toDeferredMsg, deferredSettings, defaultDeferredSettings
  -- data examination
  , data, progress, isProgress, completed, count, isEmpty, id
  -- metadata examination
  , conf, columnNames, visibleColumnNames, fieldNames, visibleFieldNames
  -- utility functions
  , dataDecoder, pathDecoder, isInitialized, notInitialized, ready
  -- commands
  , fetch, fetchFromStart, fetchDeferred, fetchDeferredFromStart, fetchCount
  , fetchCountDeferred, fetchMetadata, set, edit, save, delete
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

# Initialization, configuration
@docs initDataValueList, initList, initDataValueForm, initForm, listDecoder, formDecoder, countBaseUri,
      pageSize, countDecoder, idParam, offsetLimitParams,
      toDeferredMsg, deferredSettings, defaultDeferredSettings

# Commands
@docs fetch, fetchFromStart, fetchDeferred, fetchDeferredFromStart, fetchCount, fetchCountDeferred,
      fetchMetadata, set, edit, save, delete

# Data examination
@docs data, progress, isProgress, completed, count, isEmpty, id

# Metadata examination
@docs conf, columnNames, visibleColumnNames, fieldNames, visibleFieldNames

# Utility functions
@docs isInitialized, notInitialized, ready, dataDecoder, pathDecoder
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

import Debug exposing (log)


type alias TypeName = String


type alias SearchParams = List (String, String)


type alias Decoder value = Dict String VM.View -> String -> JD.Decoder value


type alias Encoder value = Dict String VM.View -> String -> value -> JD.Value


type alias Setter msg value = value -> Model msg value -> Model msg value


type alias Editor value = String -> Dict String VM.View -> Path -> DataValue -> value -> value


type alias Reader value = String -> Dict String VM.View -> Path -> value -> DataValue


type alias DeferredHeader = (String, String)


type alias TimeoutDeferredConfig =
  { deferredHeader: DeferredHeader
  , deferredHeaderIfTimeout: String -> Maybe DeferredHeader
  }


type alias DeferredConfig msg =
  { toMsg: DR.Tomsg msg
  , timeoutDeferredConfig: Maybe TimeoutDeferredConfig
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
  , editor: Editor value
  , reader: Reader value
  , emptyData: Data value
  , metadataBaseUri: String
  , dataBaseUri: String
  , uri: Bool -> SearchParams -> Model msg value -> String
  , saveUri: SearchParams -> Model msg value -> String
  , metadata: Dict String VM.View
  , metadataFetcher: String -> Cmd VM.Msg
  , deferredConfig: Maybe (DeferredConfig msg)
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


type Model msg value = Model (Data value) (Config msg value)


-- list types

type alias ListModel msg value = Model msg (List value)


type alias DataValueListModel msg = ListModel msg DataValue


type alias ListMsg msg value = Msg msg (List value)


type alias DataValueListMsg msg = ListMsg msg DataValue


--form types

type alias FormModel msg value = Model msg value


type alias DataValueFormModel msg = FormModel msg DataValue


type alias FormMsg msg value = Msg msg value


type alias DataValueFormMsg msg = FormMsg msg DataValue


{-| Data for dynamic models.
-}
type DataValue
  = FieldValue JD.Value
  | RecordValue (List DataValue)


{-| Path to field in dynamic model data.
-}
type Path
  = Name String Path
  | Idx Int Path
  | End


type Msg msg value
  = MetadataMsg (Maybe (Cmd msg)) VM.Msg
  | DataMsg TypeName Bool SearchParams (Result Http.Error value)
  | CountMsg TypeName SearchParams (Result Http.Error Int)
  | DeleteMsg TypeName SearchParams (Result Http.Error String)
  | EditMsg Path DataValue
  | MetadataMsgCmd (Maybe (Cmd msg))
  | UpdateCmdMsg value
  | DataCmdMsg Bool SearchParams (Maybe DeferredHeader)
  | CountCmdMsg SearchParams (Maybe DeferredHeader)
  | SaveCmdMsg SearchParams
  | DeleteCmdMsg SearchParams


type alias Tomsg msg value = Msg msg value -> msg


-- initialization & configruation

{-| Initialize [`DataValue`](JsonModel.DataValue) based list model.

      initDataValueList "/metadata" "/data" "my-view" AskMsg
-}
initDataValueList: String -> String -> String -> Ask.Tomsg msg -> DataValueListModel msg
initDataValueList metadataBaseUri dataBaseUri typeName toMessagemsg =
  let
    decoder metadata typeName = JD.list <| dataDecoder metadata typeName

    encoder metadata typeName value =
      value |>
      List.map (dataEncoder metadata typeName) |>
      JE.list

    editor typeName metadata path value data =
      let
        edit =
          case recordEditor typeName metadata path value <| RecordValue data of
            RecordValue rows -> rows

            _ -> data
      in
        case path of
          End -> edit

          Idx _ _ -> edit

          Name _ _ -> data -- list editing path cannot start with name

    reader typeName metadata path value =
      let read = fieldValue typeName metadata path <| RecordValue value
      in case path of
        End -> RecordValue value

        Idx _ _ -> read

        Name _ _ -> RecordValue [] -- list reading cannot start with name

    dataValueModel (Model data conf) =
      Model data
        { conf |
          decoder = decoder
        , encoder = encoder
        , editor = editor
        , reader = reader
        }
  in
    dataValueModel <|
      initList
        metadataBaseUri
        dataBaseUri
        typeName
        (dataDecoder Dict.empty "")
        (dataEncoder Dict.empty "")
        toMessagemsg


{-| Initialize `value` based list model.

      initList "/metadata" "/data" "my-view" decoder encoder AskMsg
-}
initList: String -> String -> String -> JD.Decoder value -> (value -> JD.Value) -> Ask.Tomsg msg -> Model msg (List value)
initList metadataBaseUri dataBaseUri typeName decoder encoder toMessagemsg =
  let
    dataDecoder _ _ = JD.list decoder

    dataEncoder _ _ value = List.map encoder value |> JE.list

    editor _ _ _ _ value = value

    reader _ _ _ _ = RecordValue []

    saveUri _ _ = ""

    emptyListData =
      Data
        []
        Nothing
        False
        []
        (Progress False False False)

    listSetter newData (Model data conf) =
      Model
        { data |
          data = data.data ++ newData
        , count = Nothing
        , completed = List.length newData < conf.pageSize
        , ready = True
        }
        conf

    listUri restart searchParams (Model data conf) =
      let
        offset =
          if not restart && searchParams == data.searchParams then
            List.length data.data
          else 0

        searchParamsWithOffsetLimit =
          List.append
            searchParams
            [ (conf.offsetParamName, toString <| offset)
            , (conf.limitParamName, toString conf.pageSize)
            ]

        queryString = Utils.httpQuery searchParamsWithOffsetLimit
      in
        conf.dataBaseUri ++ "/" ++ conf.typeName ++
        (if String.isEmpty queryString then "" else "?") ++ queryString

    countUri searchParams (Model _ conf) =
      let
        queryString = Utils.httpQuery searchParams
      in
        conf.countBaseUri ++ "/" ++ conf.typeName ++
        (if String.isEmpty queryString then "" else "?") ++ queryString
  in
    Model
      (emptyListData False) -- model not ready upon initialization
      { typeName = typeName
      , decoder = dataDecoder
      , encoder = dataEncoder
      , setter = listSetter
      , editor = editor
      , reader = reader
      , emptyData = emptyListData True -- model ready when emptyData function called
      , metadataBaseUri = metadataBaseUri
      , dataBaseUri = dataBaseUri
      , uri = listUri
      , saveUri = saveUri
      , metadata = Dict.empty
      , metadataFetcher = VM.fetchMetadata
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


{-| Initialize [`DataValue`](JsonModel.DataValue) based form model.

      initDataValueForm "/metadata" "/data" "my-view" AskMsg
-}
initDataValueForm: String -> String -> String -> Ask.Tomsg msg -> DataValueFormModel msg
initDataValueForm metadataBaseUri dataBaseUri typeName toMessagemsg =
  let
    decoder metadata typeName = dataDecoder metadata typeName

    encoder metadata typeName value = dataEncoder metadata typeName value

    editor typeName metadata path value data =
      let
        edit =
          case recordEditor typeName metadata path value data of
            RecordValue fields -> RecordValue fields

            _ -> data
      in
        case path of
          End -> edit

          Name _ _ -> edit

          Idx _ _ -> data -- form editing path cannot start with index

    reader typeName metadata path value =
      let read = fieldValue typeName metadata path value
      in case path of
        End -> value

        Name _ _ -> read

        Idx _ _ -> RecordValue [] -- form reading cannot start with index

    id (Model data conf) =
      case conf.reader conf.typeName conf.metadata (Name conf.idParamName End) data.data of
        FieldValue v ->
          JD.decodeValue JD.int v |>
          Result.toMaybe |>
          Maybe.map toString

        x -> Nothing

    dataValueModel (Model data conf) =
      Model data
       { conf |
         decoder = decoder
       , encoder = encoder
       , editor = editor
       , reader = reader
       }
  in
    dataValueModel <|
      initForm
        metadataBaseUri
        dataBaseUri
        typeName
        (dataDecoder Dict.empty "")
        (dataEncoder Dict.empty "")
        (RecordValue [])
        id
        toMessagemsg


{-| Initialize `value` based form model.

      initForm "/metadata" "/data" "my-view" decoder encoder initValue idFunction AskMsg
-}
initForm:
  String -> String -> String ->
  JD.Decoder value -> (value -> JD.Value) ->
  value -> (FormModel msg value -> Maybe String) ->
  Ask.Tomsg msg ->
  FormModel msg value
initForm metadataBaseUri dataBaseUri typeName decoder encoder initValue id toMessagemsg =
  let
    dataDecoder _ _ = decoder

    dataEncoder _ _ value = encoder value

    editor _ _ _ _ value = value -- not implemented (only for DataValue model)

    reader _ _ _ _ = RecordValue [] -- not implemented (only for DataValue model)

    setter newdata (Model data conf) =
      Model
        { data |
          data = newdata
        , count = Nothing
        , completed = True
        , ready = True
        }
        conf

    query params =
      String.join "&" <| List.map (\(k,v) -> Http.encodeUri k ++ "=" ++ Http.encodeUri v) params

    queryString params = let q = query params in if String.isEmpty q then "" else "?" ++ q

    maybeIdPathAndQueryString idParamName params =
      (case List.partition (\(n, _) -> n == idParamName) params of
        ([], pars) -> queryString pars

        ((_, id) :: tail, pars) -> "/" ++ id ++ queryString pars
      )

    formUri _ searchParams (Model data conf) =
      conf.dataBaseUri ++ "/" ++ conf.typeName ++
        (maybeIdPathAndQueryString conf.idParamName searchParams)

    saveUri searchParams ((Model data conf) as model) =
      let
        uriEnd =
            id model |>
            Maybe.map ((,) conf.idParamName) |>
            Maybe.map (flip (::) searchParams) |>
            Maybe.map (maybeIdPathAndQueryString conf.idParamName) |>
            Maybe.withDefault (maybeIdPathAndQueryString conf.idParamName searchParams)
      in
        conf.dataBaseUri ++ "/" ++ conf.typeName ++ uriEnd

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
      , decoder = dataDecoder
      , encoder = dataEncoder
      , setter = setter
      , editor = editor
      , reader = reader
      , emptyData = emptyFormData True -- model ready when emptyData function called
      , metadataBaseUri = metadataBaseUri
      , dataBaseUri = dataBaseUri
      , uri = formUri
      , saveUri = saveUri
      , metadata = Dict.empty
      , metadataFetcher = VM.fetchMetadata
      , deferredConfig = Nothing
      , countBaseUri = ""
      , countDecoder = JD.int
      , countUri = (\_ _ -> "")
      , pageSize = 0
      , idParamName = "id"
      , id = id
      , offsetParamName = "offset"
      , limitParamName = "limit"
      , toMessagemsg = toMessagemsg
      , queuedCmd = Nothing
      }


{-| Set list model decoder
-}
listDecoder: Decoder (List value) -> ListModel msg value -> ListModel msg value
listDecoder decoder (Model d conf) = Model d { conf | decoder = decoder }


{-| Set form model decoder
-}
formDecoder: Decoder value -> FormModel msg value -> FormModel msg value
formDecoder decoder (Model d conf) = Model d { conf | decoder = decoder }


{-| Set count base uri
-}
countBaseUri: String -> Model msg value -> Model msg value
countBaseUri uri (Model d conf) = Model d { conf | countBaseUri = uri }


{-| Set page size for list model - [`JsonModel.ListModel`](JsonModel#ListModel)
-}
pageSize: Int -> Model msg value -> Model msg value
pageSize pageSize (Model d conf) = Model d { conf | pageSize = pageSize }


{-| Set count decoder for list model - [`JsonModel.ListModel`](JsonModel#ListModel)
-}
countDecoder: JD.Decoder Int -> Model msg value -> Model msg value
countDecoder decoder (Model d conf) = Model d { conf | countDecoder = decoder }


{-| Set id parameter name for form model - [`JsonModel.FormModel`](JsonModel#FormModel)
-}
idParam: String -> Model msg value -> Model msg value
idParam param (Model d conf) = Model d { conf | idParamName = param }


{-| Set offset, limit parameter names for list model - [`JsonModel.ListModel`](JsonModel#ListModel)
-}
offsetLimitParams: String -> String -> Model msg value -> Model msg value
offsetLimitParams offset limit (Model d conf) =
  Model d { conf | offsetParamName = offset, limitParamName = limit }


{-| Enable sending messages to [`DeferredRequests`](DeferredRequests)
-}
toDeferredMsg: DR.Tomsg msg -> Model msg value -> Model msg value
toDeferredMsg toMsg (Model d conf) =
  Model d
    { conf |
      deferredConfig =
        case conf.deferredConfig of
          Just dc -> Just { dc | toMsg = toMsg }

          Nothing -> Just <| DeferredConfig toMsg Nothing
    }


{-| Configure deferred requests [`DeferredRequests`](DeferredRequests)

    deferredSettings toDeferredMsg ("X-Deferred", "180s") maybeDeferredHeaderOnTimeout model
-}
deferredSettings:
  DR.Tomsg msg ->
  DeferredHeader ->
  (String -> Maybe DeferredHeader) ->
  Model msg value ->
  Model msg value
deferredSettings toDeferredMsg deferredHeader deferredHeaderIfTimeout (Model d conf) =
  let
    newDefConf =
      DeferredConfig
        toDeferredMsg
        (Just <| TimeoutDeferredConfig deferredHeader deferredHeaderIfTimeout)
  in
    Model d { conf | deferredConfig = Just newDefConf }


{-| Helper function for deferred requests [`DeferredRequests`](DeferredRequests).

    defaultDeferredSettings toDeferredmsg timeout model

    maybeDeferredHeaderIfTimeout function is following:
    (\msg ->
      if String.contains
          "ERROR: canceling statement due to user request"
          msg
      then
        Just ("X-Deferred", timeout)
      else
        Nothing
    )
-}
defaultDeferredSettings: DR.Tomsg msg -> String -> Model msg value -> Model msg value
defaultDeferredSettings toDeferredmsg timeout model =
  deferredSettings
    toDeferredmsg
    ("X-Deferred", timeout)
    (\msg ->
      if String.contains
          "ERROR: canceling statement due to user request"
          msg
      then
        Just ("X-Deferred", timeout)
      else
        Nothing
    )
    model


-- Data getter functions

{-| Get model data.
-}
data: Model msg value -> value
data (Model data _) = data.data


{-| Check model [`Progress`](#Progress)
-}
progress: Model msg value -> Progress
progress (Model { progress } _) = progress


{-| Check whether on if [`Progress`](#Progress) flags is set.
-}
isProgress: Model msg value -> Bool
isProgress (Model { progress } _ ) =
  (\ { fetchProgress, countProgress, metadataProgress } ->
    fetchProgress || countProgress || metadataProgress
  ) progress


{-| Check whether all data are fetched from server. Relevant for [`ListModel`](#ListModel)
    since data are fetched in pages.
-}
completed: Model msg value -> Bool
completed (Model data _) = data.completed


{-|
-}
count: Model msg value -> Maybe Int
count (Model data _) = data.count

{-|
-}
isEmpty: ListModel msg value -> Bool
isEmpty model = List.isEmpty <| data model


{-|
-}
id: Model msg value -> Maybe String
id ((Model data conf) as model) =
  conf.id model


{-|
-}
isInitialized: Model msg value -> Bool
isInitialized model = not (notInitialized model)


{-| True if Model is metadata are empty
-}
notInitialized: Model msg value -> Bool
notInitialized (Model { progress } { metadata }) =
  Dict.isEmpty metadata || progress.metadataProgress


{-| True if data are set at least once during Model lifetime
-}
ready: Model msg value -> Bool
ready (Model data _) =
  data.ready

-- metadata examination

{-|
-}
columnNames: Model msg value -> List String
columnNames (Model _ conf as model) = fieldNames True conf.typeName model


{-|
-}
visibleColumnNames: Model msg value -> List String
visibleColumnNames (Model _ conf as model) = visibleFieldNames conf.typeName model


{-|
-}
visibleFieldNames: String -> Model msg value -> List String
visibleFieldNames typeName model =
  fieldNames False typeName model


{-| If bool parameter is True return all field labels else only visible.
-}
fieldNames: Bool -> String -> Model msg value -> List String
fieldNames all typeName (Model _ { metadata }) =
  Dict.get typeName metadata |>
  Maybe.map .fields |>
  Maybe.map (List.filter (.visible >> (||) all)) |>
  Maybe.map (List.map .label) |>
  Maybe.withDefault []

{-| Returns model configuration
-}
conf: Model msg value -> Config msg value
conf (Model _ conf) = conf


-- utility functions

{-| Decoder for [`DataValue`](#DataValue)

    dataDecoder metadata viewTypeName
-}
dataDecoder: Dict String VM.View -> String -> JD.Decoder DataValue
dataDecoder metadata viewTypeName =
  let
    fail name = JD.fail <| "Metadata not found for type: " ++ name

    fieldDecoder fieldmd =
      if (not fieldmd.isComplexType) && (not fieldmd.isCollection) then
        JD.map FieldValue JD.value
      else if fieldmd.isComplexType && (not fieldmd.isCollection) then
        Dict.get fieldmd.typeName metadata |>
        Maybe.map recordDecoder |>
        Maybe.withDefault (fail fieldmd.typeName)
      else if (not fieldmd.isComplexType) && fieldmd.isCollection then
        JD.list (JD.value |> JD.map FieldValue) |>
        JD.map RecordValue
      else
        Dict.get fieldmd.typeName metadata |>
        Maybe.map listDecoder |>
        Maybe.map (\ld -> JD.map RecordValue ld) |>
        Maybe.withDefault (fail fieldmd.typeName)

    recordDecoder viewmd =
      let
        fieldsDecoder fields decodedFields =
          case fields of
            [] -> JD.succeed <| List.reverse decodedFields

            fieldmd :: tail ->
              JD.field fieldmd.name (JD.lazy (\_ -> fieldDecoder fieldmd)) |>
              JD.andThen (\df -> fieldsDecoder tail <| df :: decodedFields)
      in
        JD.oneOf [ fieldsDecoder viewmd.fields [], JD.null [] ] |>
        JD.map RecordValue

    listDecoder viewmd = JD.oneOf [ JD.list (recordDecoder viewmd), JD.null [] ]
  in
    Dict.get viewTypeName metadata |>
    Maybe.map recordDecoder |>
    Maybe.withDefault (fail viewTypeName)


{-| Decoder of json array of strings and ints, i.e. ["department-name", 2, "employee-name"]
-}
pathDecoder: JD.Decoder Path
pathDecoder =
  let
    pathElementDecoder =
      JD.oneOf
        [ JD.string |> JD.map ((flip Name) End)
        , JD.int |> JD.map ((flip Idx) End)
        ]
  in
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


{-| Encoder for [`DataValue`](#DataValue)

    dataEncoder metadata viewTypeName value
-}
dataEncoder: Dict String VM.View -> String -> DataValue -> JD.Value
dataEncoder metadata viewTypeName value =
  let
    encodeField value = case value of
      FieldValue v -> v

      _ -> JE.null -- unexpected element, encode as null

    encodeRecord value vmd = case value of
      RecordValue values ->
        Utils.zip vmd.fields values |>
        List.map
          (\(fmd, value) ->
            ( fmd.name
            , if (not fmd.isComplexType) && (not fmd.isCollection) then
                encodeField value
              else if fmd.isComplexType && (not fmd.isCollection) then
                Dict.get fmd.typeName metadata |>
                Maybe.map (encodeRecord value) |>
                Maybe.withDefault JE.null
              else if (not fmd.isComplexType) && fmd.isCollection then
                case value of
                  RecordValue primitiveValues ->
                    primitiveValues |>
                    List.map encodeField |>
                    JE.list

                  _ -> JE.list []
              else
                Dict.get fmd.typeName metadata |>
                Maybe.map (encodeList value) |>
                Maybe.withDefault JE.null
            )
          ) |>
          JE.object

      _ -> JE.null -- unexpected element, encode as null

    encodeList value vmd = case value of
      RecordValue values ->
        values |>
        List.map ((flip encodeRecord) vmd) |>
        JE.list

      _ -> JE.null -- unexpected element, encode as null
  in
    Dict.get viewTypeName metadata |>
    Maybe.map (encodeRecord value) |>
    Maybe.withDefault JE.null


-- commands

{-| Fetch view data.

    fetch toMsg [("id", "1")]
-}
fetch: Tomsg msg value -> SearchParams -> Cmd msg
fetch toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| DataCmdMsg False searchParams Nothing


{-| Fetch view data from starting from first record. Relevant for [`ListModel`](#ListModel)

    fetch toMsg [("id", "1")]
-}
fetchFromStart: Tomsg msg value -> SearchParams -> Cmd msg
fetchFromStart toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| DataCmdMsg True searchParams Nothing


{-| Fetch view data with deferred header set. See [`DeferredConfig`](#DeferredConfig)
-}
fetchDeferred: Tomsg msg value -> SearchParams -> DeferredConfig msg -> Cmd msg
fetchDeferred toMsg searchParams deferredConfig =
  deferredConfig.timeoutDeferredConfig |>
  Maybe.map
    (\dc ->
      Task.perform toMsg <| Task.succeed <| DataCmdMsg False searchParams <| Just dc.deferredHeader
    ) |>
  Maybe.withDefault Cmd.none


{-| Fetch view data with deferred header set starting from first record.
    See [`DeferredConfig`](#DeferredConfig). Relevant for [`ListModel`](#ListModel)
-}
fetchDeferredFromStart: Tomsg msg value -> SearchParams -> DeferredConfig msg -> Cmd msg
fetchDeferredFromStart toMsg searchParams deferredConfig =
  deferredConfig.timeoutDeferredConfig |>
  Maybe.map
    (\dc ->
      Task.perform toMsg <| Task.succeed <| DataCmdMsg True searchParams <| Just dc.deferredHeader
    ) |>
  Maybe.withDefault Cmd.none


{-| Fetch record count. Command is available if [`countBaseUri`](#countBaseUri)
    is configured.
-}
fetchCount: Tomsg msg value -> SearchParams -> Cmd msg
fetchCount toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| CountCmdMsg searchParams Nothing


{-| Fetch record count with deferred header set.
    Command is available if [`countBaseUri`](#countBaseUri) is configured.
-}
fetchCountDeferred: Tomsg msg value -> SearchParams -> DeferredConfig msg -> Cmd msg
fetchCountDeferred toMsg searchParams deferredConfig =
  deferredConfig.timeoutDeferredConfig |>
  Maybe.map
    (\dc ->
      Task.perform toMsg <| Task.succeed <| CountCmdMsg searchParams <| Just dc.deferredHeader
    ) |>
  Maybe.withDefault Cmd.none


{-| Fetch view metadata.
-}
fetchMetadata: Tomsg msg value -> Cmd msg
fetchMetadata toMsg =
  Task.perform toMsg <| Task.succeed <| MetadataMsgCmd Nothing


{-| Set model value.
-}
set: Tomsg msg value -> value -> Cmd msg
set toMsg value =
  Task.perform toMsg <| Task.succeed <| UpdateCmdMsg value


{-| Edit `DataValue` model.
-}
edit: Tomsg msg value -> Path -> DataValue -> Cmd msg
edit toMsg path value =
  Task.perform toMsg <| Task.succeed <| EditMsg path value


{-| Save model.
-}
save: Tomsg msg value -> SearchParams -> Cmd msg
save toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| SaveCmdMsg searchParams


{-| Delete model.
-}
delete: Tomsg msg value -> SearchParams -> Cmd msg
delete toMsg searchParams =
  Task.perform toMsg <| Task.succeed <| DeleteCmdMsg searchParams


-- update

update: Tomsg msg value -> Msg msg value -> Model msg value -> (Model msg value, Cmd msg)
update toMsg msg (Model data conf as same) =
  let
    --model construction
    maybeWithNewData restart searchParams (Model _ conf) =
      if not restart && searchParams == data.searchParams then
        same
      else let emptyData = conf.emptyData in
        Model { emptyData | searchParams = searchParams } conf
    withProgress progress (Model d c) = Model { d | progress = progress } c
    withEmptyQueue ((Model d c) as m) =
      if c.queuedCmd == Nothing then m else Model d { c | queuedCmd = Nothing }
    --progress
    fetchProgress = Progress True data.progress.countProgress False
    fetchDone = Progress False data.progress.countProgress False
    countProgress = Progress data.progress.fetchProgress True False
    countDone = Progress data.progress.fetchProgress False False
    -- metadata progress can be within other progress, so keep other values
    metadataProgress = data.progress |> (\p -> { p | metadataProgress = True })
    metadataDone = data.progress |> (\p -> { p | metadataProgress = False })
    allDone = Progress False False False
    isFetchProgress = data.progress.fetchProgress
    isCountProgress = data.progress.countProgress
    isMetadataProgress = data.progress.metadataProgress
    --message - model integrity check
    hasIntegrity (name, progress) = name == conf.typeName && progress
    unInitialized = notInitialized same -- shortcut function

    -- metadata fetch
    fetchMetadata andThen =
      Task.perform toMsg <| Task.succeed <| MetadataMsgCmd andThen

    metadataHttpRequest maybeAndThen typeName =
      let
        mapper = MetadataMsg maybeAndThen >> toMsg
      in
        Cmd.map mapper <| conf.metadataFetcher (conf.metadataBaseUri ++ "/" ++ typeName)

    viewHttpRequest uri maybeHeader decoder =
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
          (\data ->
            (Result.mapError
              ((flip Http.BadPayload) emptyHttpResponse)
              (JD.decodeValue decoder data)
            )
          )

    maybeSubscribeDeferred dataMsgConstr integrity deferredResponse =
      (if hasIntegrity integrity then Just deferredResponse else Nothing) |>
      Maybe.map2
        ((flip DR.maybeSubscribeCmd) (toMsg << dataMsgConstr))
        (Maybe.map .toMsg conf.deferredConfig)
      |> Maybe.andThen identity
      |> Maybe.map ((,) same)

    maybeAskDeferred cmdMsgConstr integrity response progress =
      let
        askCmd toAskMsg header =
          Ask.ask toAskMsg "Timeout occurred. Try deferred request?" <|
            Task.perform toMsg <| Task.succeed <| cmdMsgConstr (Just header)
      in
        (if hasIntegrity integrity then Just 1 else Nothing) |>
        Maybe.andThen (always conf.deferredConfig) |>
        Maybe.andThen .timeoutDeferredConfig |>
        Maybe.andThen
          (\{ deferredHeaderIfTimeout } ->
            deferredHeaderIfTimeout response |>
            Maybe.map (askCmd conf.toMessagemsg)
          ) |>
        Maybe.map ((,) (withProgress progress same))

    maybeSubscribeOrAskDeferred -- withDefault method is not used because we do not need to print default error every time
      dataMsgConstr cmdMsgConstr integrity response progress default =
      case maybeSubscribeDeferred dataMsgConstr integrity response of
        Just resp -> resp

        Nothing ->
          case maybeAskDeferred cmdMsgConstr integrity response progress of
            Just resp -> resp

            Nothing -> default ()

    maybeAskDeferredOrError cmdMsgConstr integrity response progress default =
      case maybeAskDeferred cmdMsgConstr integrity response progress of
        Just resp -> resp

        Nothing -> default ()

    maybeErrorResponse progress err = \() -> errorResponse progress err

    queueCmd cmd =
      Model data { conf | queuedCmd = Just cmd }
        ! []

    maybeUnqueueCmd (Model _ conf) =
      conf.queuedCmd |>
      Maybe.map (\msg -> Task.perform toMsg <| Task.succeed msg) |>
      Maybe.withDefault Cmd.none

    errorResponse progress err =
      let
        errToString err =
          case err of
            Http.BadStatus { body } -> body

            Http.BadPayload msg { body } -> msg

            x -> toString x
      in
        log
          (toString err)
          ( same |> withProgress progress
          , Ask.error conf.toMessagemsg (errToString err)
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
              List.map (\n -> (n, VM.View "" [])) |>
              Dict.fromList |>
              Dict.union (Dict.insert view.typeName view conf.metadata)
            newModel = Model data { conf | metadata = newMetadata }
            maybeNewMetadataCmd =
              Dict.filter (\_ v -> String.isEmpty v.typeName) newMetadata |>
              Dict.keys |>
              List.head |>
              Maybe.map (metadataHttpRequest maybeCmd)
          in
            maybeNewMetadataCmd |>
            Maybe.map ((,) newModel) |>
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
                conf.setter newdata newModel |> withProgress fetchDone
            else emptyQueueModel
          , maybeUnqueueCmd same
          )

      CountMsg name searchParams (Ok count) ->
        ( if hasIntegrity (name, data.progress.countProgress) then
            let
              newModel =
                  Model
                    { data |
                      -- if search params correspond set count else reset count
                      count = if (searchParams == data.searchParams) then Just count else Nothing
                    }
                    conf
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

      DataMsg name restart searchParams (Err ((Http.BadPayload _ response) as err)) ->
        maybeSubscribeOrAskDeferred
          (DataMsg name restart searchParams <<
            mapJsonHttpResult (conf.decoder conf.metadata conf.typeName))
          (DataCmdMsg restart searchParams)
          (name, isFetchProgress)
          response.body
          fetchDone
          (maybeErrorResponse fetchDone err)

      CountMsg name searchParams (Err ((Http.BadPayload _ response) as err)) ->
        maybeSubscribeOrAskDeferred
          (CountMsg name searchParams << mapJsonHttpResult conf.countDecoder)
          (CountCmdMsg searchParams)
          (name, isCountProgress)
          response.body
          countDone
          (maybeErrorResponse countDone err)

      DataMsg name restart searchParams (Err ((Http.BadStatus response) as err)) ->
        maybeAskDeferredOrError
          (DataCmdMsg restart searchParams)
          (name, isFetchProgress)
          response.body
          fetchDone
          (maybeErrorResponse fetchDone err)

      CountMsg name searchParams (Err ((Http.BadStatus response) as err)) ->
        maybeAskDeferredOrError
          (CountCmdMsg searchParams)
          (name, isCountProgress)
          response.body
          countDone
          (maybeErrorResponse countDone err)

      DataMsg _ _ _ (Err err) -> errorResponse fetchDone err

      CountMsg _ _ (Err err) -> errorResponse countDone err

      DeleteMsg _ _ (Err err) -> errorResponse fetchDone err

      MetadataMsg _ (VM.ViewMetadataMsg (Err err) _) -> errorResponse allDone err

      EditMsg path value ->
        if unInitialized then
          ( same, fetchMetadata (Just <| edit toMsg path value) )
        else
          let
            good =
              hasIntegrity
                ( conf.typeName
                , not
                    ( isFetchProgress ||
                      isCountProgress ||
                      isMetadataProgress
                    )
                )
          in
            if good then
              let
                newValue = conf.editor conf.typeName conf.metadata path value data.data
              in
                ( Model { data | data = newValue } conf
                , Cmd.none
                )
            else ( same, Cmd.none )

      MetadataMsgCmd maybeAndThen ->
        if isMetadataProgress then
          same ! []
        else
          (same |> withProgress metadataProgress)
            ! [ metadataHttpRequest maybeAndThen conf.typeName ]

      UpdateCmdMsg value ->
        if isFetchProgress then
          queueCmd <| UpdateCmdMsg value
        else
          let
            cmd =
              Task.perform
                (toMsg << DataMsg conf.typeName True data.searchParams)
                (Task.succeed <| Ok value)
          in
            (same |> withEmptyQueue |> withProgress fetchProgress)
              ! (if unInitialized then [ fetchMetadata <| Just cmd ] else [ cmd ])


      DataCmdMsg restart searchParams deferredHeader ->
        if isFetchProgress then
          queueCmd <| DataCmdMsg restart searchParams deferredHeader
        else
          let
            cmd =
              Http.send
                (toMsg << DataMsg conf.typeName restart searchParams) <|
                viewHttpRequest (conf.uri restart searchParams same) deferredHeader <|
                  conf.decoder conf.metadata conf.typeName

            newModel = same |> withEmptyQueue |> withProgress fetchProgress
          in
            newModel
              ! (if unInitialized then [ fetchMetadata <| Just cmd ] else [ cmd ])

      CountCmdMsg searchParams deferredHeader ->
        if String.isEmpty conf.countBaseUri then
          ( same, Ask.warn conf.toMessagemsg "Cannot calculate count, count uri empty" )
        else if isCountProgress then
          queueCmd <| CountCmdMsg searchParams deferredHeader
        else
          let
            cmd =
              Http.send
                (toMsg << CountMsg conf.typeName searchParams) <|
                viewHttpRequest (conf.countUri searchParams same) deferredHeader conf.countDecoder
          in
            let
              newModel = same |> withEmptyQueue |> withProgress countProgress
            in
              newModel
                ! (if unInitialized then [ fetchMetadata <| Just cmd ] else [ cmd ])

      SaveCmdMsg searchParams ->
        if isFetchProgress then
          ( same, Ask.warn conf.toMessagemsg "Operation in progress, please try later." )
        else
          let
            method = id same |> Maybe.map (always "PUT") |> Maybe.withDefault "POST"

            value = conf.encoder conf.metadata conf.typeName data.data

            decoder = conf.decoder conf.metadata conf.typeName

            cmd =
              Http.send
                (toMsg << DataMsg conf.typeName False searchParams) <|
                saveHttpRequest (conf.saveUri searchParams same) method value decoder
          in
            (same |> withProgress fetchProgress)
              ! [ cmd ]

      DeleteCmdMsg searchParams ->
        if isFetchProgress then
          same
            ! [ Ask.warn conf.toMessagemsg "Operation in progress, please try later." ]
        else
          let
            cmd =
              Http.send
                (toMsg << DeleteMsg conf.typeName searchParams) <|
                deleteHttpRequest <| conf.saveUri searchParams same
          in
            (same |> withProgress fetchProgress)
              ! [ cmd ]


{- private function -}
recordEditor: String -> Dict String VM.View -> Path -> DataValue -> DataValue -> DataValue
recordEditor typeName metadata path value data =
  let
    vmd fmd default =
      if fmd.isComplexType then
        Dict.get fmd.typeName metadata |> Maybe.withDefault default
      else default

    setRow path viewmd idx rows =
      ( rows |>
        List.indexedMap
          (\i val -> if i == idx then transform path val viewmd else val)
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

    transform path data viewmd =
      case path of
        End -> value

        Name name rest ->
          case data of
            RecordValue values ->
              RecordValue
                ( Utils.zip viewmd.fields values |>
                  List.map
                    (\(f, val) -> -- find field transformer must be applied to
                      if f.name == name then transform rest val (vmd f viewmd) else val
                    )
                )

            fieldValue -> fieldValue -- do nothing since element must match complex type

        Idx idx End ->
          case data of
            RecordValue rows ->
              RecordValue <|
                if idx < 0 then -- insert or delete row with index -idx - 1
                  case value of
                    RecordValue [] -> deleteRow rows (-idx - 1) -- no data in value, delete row

                    _ -> insertRow value rows (-idx - 1)
                else setRow End viewmd idx rows

            fieldValue -> fieldValue -- do nothing since element must be record

        Idx idx rest ->
          case data of
            RecordValue rows -> RecordValue <| setRow rest viewmd idx rows

            fieldValue -> fieldValue -- do nothing since element must match complex type
  in
    Dict.get typeName metadata |>
    Maybe.map (transform path data) |>
    Maybe.withDefault data


{- private function -}
fieldValue: String -> Dict String VM.View -> Path -> DataValue -> DataValue
fieldValue typeName metadata path data =
  let
    reader path data viewmd =
      case path of
        Name name rest ->
          case data of
            RecordValue values ->
              Utils.zip values viewmd.fields |>
              List.filter (\(_, f) -> f.name == name) |>
              List.head |>
              Maybe.andThen
                (\(v, f) ->
                  if f.isComplexType then
                    Dict.get f.typeName metadata |> Maybe.map (reader rest v)
                  else Just v
                ) |>
              Maybe.withDefault (RecordValue [])

            _ -> RecordValue [] -- cannot match name, return empty data

        Idx idx rest ->
          case data of
            RecordValue values ->
              Utils.at idx values |>
              Maybe.map (\v -> reader rest v viewmd) |>
              Maybe.withDefault (RecordValue [])

            _ -> RecordValue [] -- cannot match idx, return empty data

        End ->
          data
  in
    Dict.get typeName metadata |>
    Maybe.map (reader path data) |>
    Maybe.withDefault (RecordValue [])
