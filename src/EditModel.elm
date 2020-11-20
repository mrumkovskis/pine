module EditModel exposing
  ( Input, Controller (..), ModelUpdater, InputValidator, Formatter, SelectInitializer
  , EditModel, JsonEditMsg, Tomsg, ControllerInitializer
  , Msgs, SelectMsgs, UpdateValue (..), ValidationResult (..), validateAll, validateAllMsg
  , init, initJsonForm, initJsonQueryForm
  , defaultController, jsonCtls, keyFromPath, withParser, overrideValidator, withFormatter, withSelectInitializer
  , withValidator, withUpdater, withInputCmd
  , setModelUpdater, setFormatter, setSelectInitializer, setInputValidator
  , fetch, fetchMsg, set, setMsg, create, createMsg, save, saveMsg, sync, syncMsg, validate, delete
  , id, data, inpValue, inp, inps, inpsByPattern, inpsTableByPattern
  , simpleCtrl, simpleSelectCtrl, noCmdUpdater, controller, inputMsg, onInputMsg, onInputCmd
  , jsonEditMsg, jsonDeleteMsg
  , update
  )

{-| Binding layer between [`Input`](#Input) representing form input field and [`JsonModel`](JsonModel).
[Controller](#Controller) functions are responsible for binding.

# Commands
@docs fetch, set, create, http, save, delete

# Utility
@docs id, noCmd, input

# Types
@docs Controller, EditModel, Input, InputWithAttributes, Msg, Tomsg

@docs update
-}


import JsonModel as JM
import ViewMetadata as VM
import Ask
import Select exposing (..)
import SelectEvents as SE
import Utils exposing (..)

import Dict exposing (..)
import Json.Encode as JE
import Json.Decode as JD
import Task exposing (..)


{-| Represents form input field. Is synchronized with model. -}
type alias Input msg =
  { name: String
  , path: JM.Path
  , value: String
  , editing: Bool
  , error: Maybe String
  , select: Maybe (SelectModel msg JM.JsonValue)
  , msgs: Maybe (Msgs msg)
  , idx: Int -- index of input in metadata fields list, used for JsonValue model
  , field: Maybe VM.Field
  , resolving: Bool
  , readonly: Bool
  }


type alias ModelInitializer msg model =
  JM.FormModel msg model -> Maybe (Dict String (Controller msg), Dict String (Input msg))


type alias ModelUpdater msg model = Input msg -> model -> UpdateValue model


type UpdateValue model
  = UpdateValue model
  -- Task value tuple first parameter is actual value, second is model updater function where
  -- first parameter is previously mentioned task value, second current model.
  | UpdateTask (Task String (model, (model -> model -> model)))


type ValidationResult
  = ValidationResult (List (String, String))
  | ValidationTask (Task String (List (String, String)))


{-| Validates input -}
type alias InputValidator msg = Input msg -> JM.JsonValue -> ValidationResult


{- Get input field text from model. Function is called when value is selected from list or model
   data are refreshed from update or fetch messages. Bool argument depends on EditModel.isEditable field
   so data can be formatted according to display mode.
-}
type alias Formatter model = model -> String


type alias SelectInitializer msg =
  Select.Tomsg msg JM.JsonValue -> -- toSelectmsg
  Ask.Tomsg msg -> -- toMessagemsg
  Input msg ->
  (String -> msg) -> -- select msg
  JM.JsonValue ->
  (SelectModel msg JM.JsonValue, Cmd msg)


{-| Controller. Binds [`Input`](#Input) together with [JsonModel](JsonModel) -}
type Controller msg =
  Controller
    { name: String
    , updateModel: ModelUpdater msg JM.JsonValue -- called on OnSelect, OnFocus _ False
    , formatter: Formatter JM.JsonValue
    , selectInitializer: Maybe (SelectInitializer msg) -- called on OnFocus _ True
    , validateInput: InputValidator msg -- called on OnMsg, OnSelect
    , inputCmd: Maybe (String -> Cmd msg)
    }


type alias ControllerInitializer msg =
  JM.Path -> VM.Field -> Maybe (Controller msg)


type alias Msgs msg =
  { onInput: String -> msg
  , onFocus: msg
  , onBlur: msg
  , onEnter: msg
  , updatemsg: String -> msg
  , clearmsg: Maybe msg
  , selectmsgs: Maybe (SelectMsgs msg)
  }


type alias SelectMsgs msg =
  { navigationmsg: Bool -> SE.Msg -> msg
  , setActivemsg: Int -> msg
  , selectmsg: Int -> msg
  }


{-| Edit model -}
type alias EditModel msg =
  { model: JM.FormModel msg JM.JsonValue
  , initializer: ModelInitializer msg JM.JsonValue
  , controllers: Dict String (Controller msg)
  , inputs: Dict String (Input msg)
  , toMessagemsg: Ask.Tomsg msg
  --, validate: Tomsg msg model -> model -> (Result String model, Cmd msg)
  --, error: Maybe String
  , isSaving: Bool
  , isDeleting: Bool
  --, isValidating: Bool
  , isEditable: Bool
  , isDirty: Bool
  }


{-| Edit model update messages -}
type JsonEditMsg msg
  = UpdateModelMsg Bool (JM.FormMsg msg JM.JsonValue)
  | FetchModelMsg (JM.FormMsg msg JM.JsonValue)
  | SaveModelMsg (JM.FormMsg msg JM.JsonValue)
  | CreateModelMsg (JM.JsonValue -> JM.JsonValue) (JM.FormMsg msg JM.JsonValue)
  | DeleteModelMsg (JM.FormMsg msg JM.JsonValue)
  -- select components messages
  | SelectMsg (Controller msg) (Select.Msg msg JM.JsonValue)
  -- input fields event messages
  | OnMsg (Controller msg) String
  | OnFocusMsg (Controller msg) Bool
  | FocusNoSearchMsg (Controller msg)
  | OnSelectMsg (Controller msg) String
  | OnSelectFieldMsg String String
  | ValidatePathMsg String
  | ValidateFieldMsg
      (Controller msg)
      (Maybe (String, (Result String (List (String, String))))) -- used for validation task
  | ValidateAllMsg
  | ValidateAllUpdateMsg (Result String (List (List (String, String))))
  | UpdateFieldMsg (Controller msg) (Result String (JM.JsonValue, JM.JsonValue -> JM.JsonValue -> JM.JsonValue))
  -- update entire model
  | EditModelMsg (JM.JsonValue -> JM.JsonValue)
  | NewModelMsg JM.SearchParams (JM.JsonValue -> JM.JsonValue)
  | SyncModelMsg
  | SubmitModelMsg
  --
  | CmdChainMsg (List (JsonEditMsg msg)) (Cmd msg) (Maybe (JsonEditMsg msg))


{-| Edit model message constructor -}
type alias Tomsg msg = (JsonEditMsg msg -> msg)


{-| Initializes model
-}
init: JM.FormModel msg JM.JsonValue -> List (String, Controller msg) -> Ask.Tomsg msg -> EditModel msg
init model ctrlList toMessagemsg =
  let
    controllers =
      ctrlList |>
      List.map (\(k, Controller c) -> (k, Controller { c | name = k })) |>
      Dict.fromList

    path =
      JD.decodeString JM.pathDecoder >> Result.withDefault JM.End

    inputs =
      controllers |>
      Dict.map
        (\k _ ->
          Input k (path k) "" False Nothing Nothing Nothing -1 Nothing False False
        )
  in
    EditModel
      model
      (always Nothing)
      controllers
      inputs
      toMessagemsg
      False
      False
      True
      False


initJsonForm:
  String -> String -> ControllerInitializer msg -> String -> Ask.Tomsg msg -> EditModel msg
initJsonForm =
  initJsonFormInternal .fields


initJsonQueryForm:
  String -> String -> ControllerInitializer msg -> String -> Ask.Tomsg msg -> EditModel msg
initJsonQueryForm =
  initJsonFormInternal .filter


initJsonFormInternal:
  (VM.View -> List VM.Field) -> String -> String -> ControllerInitializer msg -> String ->
  Ask.Tomsg msg -> EditModel msg
initJsonFormInternal fieldGetter metadataBaseUri dataBaseUrl initializer typeName toMessagemsg =
  let
    jsonFormInitializer (JM.Model _ _ as formModel) =
      JM.flattenJsonForm fieldGetter formModel |>
      List.indexedMap
        (\i (path, field, _) ->
          let
            key =
              keyFromPath path

            ctrl =
              initializer path field |>
              Maybe.withDefault (defaultController dataBaseUrl path field)

            inpVal =
              ctrl |> (\(Controller { formatter }) -> formatter <| JM.data formModel)
          in
            ( ( key, ctrl )
            , ( key, Input key path inpVal False Nothing Nothing Nothing i (Just field) False False )
            )
        ) |>
        List.unzip |>
        Tuple.mapBoth Dict.fromList Dict.fromList
  in
    EditModel
      (JM.initJsonValueForm fieldGetter metadataBaseUri dataBaseUrl typeName toMessagemsg)
      (jsonFormInitializer >> Just)
      Dict.empty
      Dict.empty
      toMessagemsg
      False
      False
      True
      False


defaultController: String -> JM.Path -> VM.Field -> Controller msg
defaultController dataBaseUrl path field =
  let
    key =
      keyFromPath path

    updater cinp model =
      let
        maybeUpdateSubList val mod =
          JM.jsonReader path mod |>
          Maybe.map
            (\exv -> case exv of
              JM.JsList vals ->
                Utils.find ((==) val) vals |>
                Maybe.map (\_ -> mod) |>
                Maybe.withDefault
                  (JM.jsonEditor (JM.appendPath path <| JM.EndIdx JM.End) val mod)

              _ -> JM.jsonEditor path val mod
            )
      in
        if field.isComplexType then
          let
            taskUpdater val mod =
              if field.isCollection then
                -- set value if value not exists already
                maybeUpdateSubList val mod |>
                Maybe.withDefault mod
              else
                JM.jsonEditor path val mod
          in
            UpdateTask
              ( Utils.httpGetJson
                  ( dataBaseUrl ++ "/create/" ++ field.typeName ++
                    Utils.httpQuery [(field.name, cinp.value)]
                  )
                  JM.jsonDecoder |>
                Task.mapError Utils.httpErrorToString |>
                Task.map (\v -> (v, taskUpdater))
              )
        else if field.isCollection then
          ( .value >> JM.stringToJsonValue field.jsonType ) cinp |>
          Maybe.andThen (\v -> maybeUpdateSubList v model) |>
          Maybe.withDefault model |>
          UpdateValue
        else
          ( .value >> JM.stringToJsonValue field.jsonType ) cinp |>
          Maybe.map (\iv -> JM.jsonEditor path iv model) |>
          Maybe.withDefault model |>
          UpdateValue

    formatter model =
      JM.jsonReader path model |>
      Maybe.map JM.jsonValueToString |>
      Maybe.withDefault ""

    validator { name, value } _ =
      let
        success k =
          [( k, "" )]

        isOk result =
          List.isEmpty result || List.all (Tuple.second >> (==) "") result

        typeValidator t =
          case t of
            "number" ->
              let
                res = Maybe.map (\_ -> value) >> Result.fromMaybe ("Not a number: " ++ value)
              in
                field.fractionDigits |>
                Maybe.map
                  (\fd ->
                    if fd > 0 then String.toFloat value |> res else String.toInt value |> res
                  ) |>
                Maybe.withDefault (String.toInt value |> res) |>
                (\r ->
                  case r of
                    Ok _ ->
                      success name

                    Err err ->
                      [ (key, err) ]
                )

            "boolean" ->
              String.toLower value |>
                (\s ->
                  if String.isEmpty s || s == "true" || s == "false" then
                    success name
                  else
                    [ (key, "Not a boolean: " ++ value) ]
                )

            _ -> success name

        enumValidator en =
          if String.isEmpty value then
            success name
          else
            Utils.find ((==) value) en |>
            Maybe.map (\_ -> success name) |>
            Maybe.withDefault [ (name, "Value must come from list") ]

        requiredValidator =
          if (field.required || not field.nullable) && String.isEmpty value then
            [ (name, "Field is mandatory") ]
          else
            []

      in
        ( case requiredValidator of
            [] ->
              typeValidator field.jsonType |>
              (\r ->
                if isOk r then
                  field.enum |>
                  Maybe.map enumValidator |>
                  Maybe.withDefault (success name)
                else
                  r
              )

            err -> 
              err
        ) |>
        ValidationResult
  in
    Controller
      { name = key
      , updateModel = updater
      , formatter = formatter
      , selectInitializer = Nothing
      , validateInput = validator
      , inputCmd = Nothing
      }


jsonCtls: List (String, Controller msg -> Controller msg) -> ControllerInitializer msg -> ControllerInitializer msg
jsonCtls ctls default path field =
  default path field |>
  Maybe.map
    (\ctl ->
      Utils.find (\(p, _) -> JM.pathMatchesPath p path) ctls |>
      Maybe.map (\(_, initFun) -> initFun ctl) |>
      Maybe.withDefault ctl
    )


keyFromPath: JM.Path -> String
keyFromPath path =
  case path of
    JM.Name p JM.End ->
      p

    JM.Idx idx JM.End ->
      String.fromInt idx

    _ ->
      JM.pathEncoder path |> JE.encode 0


withParser: (Input msg -> Input msg) -> Controller msg -> Controller msg
withParser parser (Controller ({ updateModel } as ctrl)) =
  Controller
    { ctrl |
      updateModel = \input model -> updateModel (parser input) model
    }


overrideValidator: InputValidator msg -> Controller msg -> Controller msg
overrideValidator validator (Controller ctrl) =
  Controller
    { ctrl |
      validateInput = validator
    }


withValidator: InputValidator msg -> Controller msg -> Controller msg
withValidator validator (Controller ({ validateInput } as ctrl)) =
  Controller
    { ctrl |
      validateInput = validatorChain validateInput validator
    }


withFormatter: (JM.JsonValue -> String) -> Controller msg -> Controller msg
withFormatter formatter (Controller ({ name } as ctrl)) =
  Controller
    { ctrl |
      formatter = JM.jsonValue name >> Maybe.map formatter >> Maybe.withDefault ""
    }


withSelectInitializer: SelectInitializer msg -> Controller msg -> Controller msg
withSelectInitializer selInitializer (Controller ctrl) =
  Controller
    { ctrl |
      selectInitializer = Just selInitializer
    }


withUpdater: ModelUpdater msg JM.JsonValue -> Controller msg -> Controller msg
withUpdater updater (Controller ctrl) =
  Controller
    { ctrl |
      updateModel = updater
    }


withInputCmd: (String -> Cmd msg) -> Controller msg -> Controller msg
withInputCmd inpCmd (Controller ctrl) =
  Controller
    { ctrl |
      inputCmd = Just inpCmd
    }


setModelUpdater: String -> ModelUpdater msg JM.JsonValue -> EditModel msg -> EditModel msg
setModelUpdater key updater model =
  updateController key (withUpdater updater) model


setFormatter: String -> Formatter JM.JsonValue -> EditModel msg -> EditModel msg
setFormatter key formatter model =
  updateController key (\(Controller c) -> Controller { c | formatter = formatter }) model


setSelectInitializer: String -> Maybe (SelectInitializer msg) -> EditModel msg -> EditModel msg
setSelectInitializer key initializer model =
  updateController
    key
    (\(Controller c) -> Controller { c | selectInitializer = initializer }) model


setInputValidator: String -> InputValidator msg -> EditModel msg -> EditModel msg
setInputValidator key validator model =
  updateController key (withValidator validator) model


updateController: String -> (Controller msg -> Controller msg) -> EditModel msg -> EditModel msg
updateController key updater model =
  Dict.get key model.controllers |>
  Maybe.map updater |>
  Maybe.map (\c -> Dict.insert key c model.controllers) |>
  Maybe.map (\cs -> { model | controllers = cs}) |>
  Maybe.withDefault model


{-| Chain validators. When both validators perform error on then field, left is taken. -}
validatorChain: InputValidator msg -> InputValidator msg -> InputValidator msg
validatorChain validator1 validator2 input model =
  let
    mergeValidations r1 r2 =
      Dict.merge
        Dict.insert
        (\k vr1 vr2 r ->
          if String.isEmpty vr1 then Dict.insert k vr2 r else Dict.insert k vr1 r
        )
        Dict.insert
        (Dict.fromList r1)
        (Dict.fromList r2)
        Dict.empty |>
      Dict.toList

    vres1 =
      validator1 input model

    vres2 =
      validator2 input model
  in
    case vres1 of
      ValidationResult res1 ->
        case vres2 of
          ValidationResult res2 ->
            mergeValidations res1 res2 |> ValidationResult

          ValidationTask rest2 ->
            rest2 |> Task.map (\r -> mergeValidations res1 r) |> ValidationTask

      ValidationTask rest1 ->
        case vres2 of
          ValidationResult res2 ->
            rest1 |> Task.map (\r -> mergeValidations r res2) |> ValidationTask

          ValidationTask rest2 ->
            rest1 |>
            Task.andThen (\r1 -> Task.map (\r2 -> mergeValidations r1 r2) rest2) |>
            ValidationTask


{-| Fetch data by id from server. Calls [`JsonModel.fetch`](JsonModel#fetch)
-}
fetch: Tomsg msg -> Int -> Cmd msg
fetch toMsg fid =
  JM.fetch (toMsg << FetchModelMsg) <| [ ("id", String.fromInt fid) ]


fetchMsg: Tomsg msg -> Int -> msg
fetchMsg toMsg fid =
  JM.fetchMsg (toMsg << FetchModelMsg) <| [ ("id", String.fromInt fid) ]


{-| Set model data. After updating inputs, calls [`JsonModel.set`](JsonModel#set)
-}
set: Tomsg msg -> (JM.JsonValue -> JM.JsonValue) -> Cmd msg
set toMsg editFun =
  domsg <| setMsg toMsg editFun


setMsg: Tomsg msg -> (JM.JsonValue -> JM.JsonValue) -> msg
setMsg toMsg editFun =
  toMsg <| EditModelMsg editFun


{-| Creates model data, calling [`JsonModel.set`](JsonModel#create).
After that call function `createFun` on received data.
-}
create: Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> Cmd msg
create toMsg createParams createFun =
  domsg <| createMsg toMsg createParams createFun


createMsg: Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> msg
createMsg toMsg createParams createFun =
  toMsg <| NewModelMsg createParams createFun


{-| Save model to server.  Calls [`JsonModel.save`](JsonModel#save)
-}
save: Tomsg msg -> Cmd msg
save =
  domsg << saveMsg


saveMsg: Tomsg msg -> msg
saveMsg toMsg =
  toMsg SubmitModelMsg


sync: Tomsg msg -> Cmd msg
sync =
  domsg << syncMsg


syncMsg: Tomsg msg -> msg
syncMsg toMsg =
  toMsg <| SyncModelMsg

validate: Tomsg msg -> String -> Cmd msg
validate toMsg path =
  domsg (toMsg <| ValidatePathMsg path )

validateAll: Tomsg msg -> Cmd msg
validateAll =
  domsg << validateAllMsg

validateAllMsg: Tomsg msg -> msg
validateAllMsg toMsg =
  toMsg <| ValidateAllMsg


{-| Save model from server.  Calls [`JsonModel.delete`](JsonModel#delete)
-}
delete: Tomsg msg -> Int -> Cmd msg
delete toMsg did =
  JM.delete (toMsg << DeleteModelMsg) [("id", String.fromInt did)]


{-| Gets model id.  Calls [`JsonModel.id`](JsonModel#id) and tries to convert result to `Int`
-}
id: EditModel msg -> Maybe Int
id =
  .model >> JM.id >> Maybe.andThen String.toInt


{-| Gets model data.
-}
data: EditModel msg -> JM.JsonValue
data { model } =
  JM.data model

{-| Gets input value by input name -}
inpValue: String -> EditModel msg -> Maybe String
inpValue name { inputs } =
  Dict.get name inputs |>
  Maybe.map .value

{-| Creates simple controller -}
simpleCtrl: (String -> JM.JsonValue -> JM.JsonValue) -> Formatter JM.JsonValue -> Controller msg
simpleCtrl updateModel formatter =
  Controller
    { name = ""
    , updateModel = noCmdUpdater updateModel
    , formatter = formatter
    , selectInitializer = Nothing
    , validateInput = \_ _ -> ValidationResult []
    , inputCmd = Nothing
    }


{-| Creates simple controller with select list -}
simpleSelectCtrl: (String -> JM.JsonValue -> JM.JsonValue) -> Formatter JM.JsonValue -> SelectInitializer msg -> Controller msg
simpleSelectCtrl updateModel formatter selectInitializer =
  Controller
    { name = ""
    , updateModel = noCmdUpdater updateModel
    , formatter = formatter
    , selectInitializer = Just selectInitializer
    , validateInput = \_ _ -> ValidationResult []
    , inputCmd = Nothing
    }


noCmdUpdater: (String -> model -> model) -> ModelUpdater msg model
noCmdUpdater updater =
  \input mod -> UpdateValue <| updater input.value mod


{-| Creates controller -}
controller:
  ModelUpdater msg JM.JsonValue ->
  Formatter JM.JsonValue ->
  Maybe (SelectInitializer msg) ->
  InputValidator msg ->
  Maybe (String -> Cmd msg) ->
  Controller msg
controller updateModel formatter selectInitializer validator inputCmd =
  Controller
    { name = ""
    , updateModel = updateModel
    , formatter = formatter
    , selectInitializer = selectInitializer
    , validateInput = validator
    , inputCmd = inputCmd
    }


{-| Gets input from model for rendering. Function furnishes input with attributes
    using `InputAttrs`
-}
inp: String -> Tomsg msg -> EditModel msg -> Maybe (Input msg)
inp key toMsg { controllers, inputs } =
  Maybe.map2 (inpInternal toMsg)
    (Dict.get key controllers)
    (Dict.get key inputs)


{-| Gets inputs from model for rendering
-}
inps: List String -> Tomsg msg -> EditModel msg -> List (Input msg)
inps keys toMsg model =
  List.foldl
    (\k r -> inp k toMsg model |> Maybe.map (\i -> i :: r) |> Maybe.withDefault r)
    []
    keys |>
  List.reverse


inpsByPattern: String -> Tomsg msg -> EditModel msg -> List (Input msg)
inpsByPattern pattern toMsg { controllers, inputs } =
  let
    patternArr = String.words pattern
  in
    Dict.filter (\_ i -> JM.pathMatches patternArr i.path) inputs |>
    Dict.values |>
    List.sortBy .idx |>
    List.concatMap
      (\i ->
        Dict.get i.name controllers |>
        Maybe.map (\c -> inpInternal toMsg c i) |>
        Maybe.map List.singleton |>
        Maybe.withDefault []
      )


inpsTableByPattern: Tomsg msg -> List String -> EditModel msg -> List (List (Input msg))
inpsTableByPattern toMsg patterns model =
  patterns |>
  List.map (\p -> inpsByPattern p toMsg model) |>
  Utils.transpose


inpInternal: Tomsg msg -> Controller msg -> Input msg -> Input msg
inpInternal toMsg ctl input =
  let
    onEnter = syncMsg toMsg -- update model from input when enter is pressed

    msgs =
      { onInput = toMsg << OnMsg ctl
      , onFocus = toMsg <| OnFocusMsg ctl True
      , onBlur = toMsg <| OnFocusMsg ctl False
      , onEnter = onEnter
      , updatemsg = toMsg << OnSelectMsg ctl
      , clearmsg =
          ctl |>
          (\(Controller { selectInitializer }) ->
            selectInitializer |> Maybe.map (\_ -> toMsg <| OnSelectMsg ctl "")
          )
      , selectmsgs =
          input.select |>
          Maybe.map
            ( let
                toSMsg = toMsg << SelectMsg ctl
              in
                always
                  { navigationmsg =
                      \isActive ->
                        if isActive then
                          navigationMsg toSMsg
                        else
                          \ev ->
                            case ev of
                              SE.Select -> onEnter

                              x -> navigationMsg toSMsg x
                  , setActivemsg = setActiveMsg toSMsg
                  , selectmsg =  selectMsg toSMsg
                  }
            )
      }
  in
    { input | msgs = Just msgs }


{-| Produces `OnMsg` input message. This can be used to set or clear text in input field.
-}
onInputMsg: String -> Tomsg msg -> EditModel msg -> Maybe (String -> msg)
onInputMsg key toMsg { controllers } =
  Dict.get key controllers |>
  Maybe.map (\ctrl -> toMsg << OnMsg ctrl)


{-| Produces `OnSelectMsg` input message which updates model from input.
    And then executes command. This function can be used to set controller's `inputCmd`
-}
onInputCmd: String -> Tomsg msg -> Cmd msg -> String -> Cmd msg
onInputCmd key toMsg cmd value =
  do toMsg <| CmdChainMsg [ OnSelectFieldMsg key value ] cmd Nothing


{-| Produces `OnSelectMsg` input message which updates model from input.
  This can be used on input events like `onCheck` or `onClick` to update model from input
-}
inputMsg: String -> Tomsg msg -> EditModel msg -> Maybe (String -> msg)
inputMsg key toMsg { controllers } =
  Dict.get key controllers |>
  Maybe.map (\ctrl -> toMsg << OnSelectMsg ctrl)


{-| Produces `EditModelMsg` message given json encoded `Path`
-}
jsonEditMsg: Tomsg msg -> String -> JM.JsonValue -> msg
jsonEditMsg toMsg path value =
  toMsg <| EditModelMsg (\m -> JM.jsonEdit path value m)


{-| Produces `EditModelMsg` message given json encoded `Path` with `JsonValue` `JsNull`
-}
jsonDeleteMsg: Tomsg msg -> String -> msg
jsonDeleteMsg toMsg path =
  toMsg <| EditModelMsg (\m -> JM.jsonEdit path JM.JsNull m)


{-| Model update -}
update: Tomsg msg -> JsonEditMsg msg -> EditModel msg -> (EditModel msg, Cmd msg)
update toMsg msg ({ model, inputs, controllers, toMessagemsg } as same) =
  let
    updateModelFromInput newInputs ctrl input =
      let
        mod = JM.data model
      in
        if ctrl.formatter mod /= input.value then
          case ctrl.updateModel input mod of
            UpdateValue nmod ->
              ( { same | inputs = newInputs, isDirty = True }
              , do toMsg <|
                  CmdChainMsg
                    [ EditModelMsg (always nmod), ValidateFieldMsg (Controller ctrl) Nothing ]
                    Cmd.none
                    Nothing
              )

            UpdateTask task ->
              ( { same |
                  inputs =
                    Dict.update
                      input.name
                      (Maybe.map (\i -> { i | resolving = True }))
                      newInputs
                , isDirty = True
                }
              , Task.attempt (toMsg << UpdateFieldMsg (Controller ctrl)) task
              )

        else
          ( { same | inputs = newInputs }, Cmd.none )

    updateModelFromActiveInput =
      Dict.values >>
      List.filter .editing >>
      List.head >>
      Maybe.andThen
        (\input -> Dict.get input.name controllers |> Maybe.map (\(Controller c) -> (c, input))) >>
      Maybe.map (\(c, input) -> updateModelFromInput inputs c input) >>
      Maybe.withDefault (same, Cmd.none)

    validationUpdater defName = 
      List.foldl
        (\(name, err) newInputs ->
          Dict.update
            (if String.isEmpty name then defName else name)
            (Maybe.map
              (\i -> { i | error = if String.isEmpty err then Nothing else Just err })
            )
            newInputs
        )

    updateValidationResults defName res =
        { same |
          inputs =
            validationUpdater defName
              inputs
              (if List.isEmpty res then [ (defName, "") ] else res)
        }


    updateInput ctrl value newInputs =
      Dict.get ctrl.name newInputs |>
      Maybe.map
        (\input ->
          let
            ninput =
              { input |
                value = value
              , select = input.select |> Maybe.map (Select.updateSearch value)
              }
          in
            ( Dict.insert ctrl.name ninput newInputs, Just ninput )
        ) |>
      Maybe.withDefault (newInputs, Nothing)

    onInput toSelectmsg ctrl value = -- OnMsg
      updateInput ctrl value inputs |>
      (\(ninps, mbinp) ->
        ( { same | inputs = ninps }
        , mbinp |> Maybe.andThen .select |>
          Maybe.map (\_ -> [ Select.search toSelectmsg value ]) |> -- select search command
          Maybe.withDefault [] |>
          (\cmds ->
            ctrl.inputCmd |>
            Maybe.map (\fcmd -> fcmd value) |>
            Maybe.map -- input command
              (\cmd ->
                (if cmd == Cmd.none then cmds else cmd :: cmds) |> Cmd.batch
              ) |>
            Maybe.withDefault (Cmd.batch cmds)
          )
        )
      )

    setEditing ctrl focus maybeDoSearch =  -- OnFocus
      let

        processFocusSelect input =
          if focus && not input.readonly then
            ctrl.selectInitializer |>
            Maybe.map
              (\initializer ->
                initializer
                  (toMsg << SelectMsg (Controller ctrl))
                  same.toMessagemsg
                  input
                  (toMsg << OnSelectMsg (Controller ctrl))
                  (JM.data model)
              ) |>
            Maybe.map
              (Tuple.mapBoth
                (\sel -> { input | editing = True, select = Just sel })
                (\cmd -> if maybeDoSearch then cmd else Cmd.none)
              ) |>
            Maybe.withDefault ({ input | editing = True, select = Nothing }, Cmd.none)
          else ({ input | editing = False, select = Nothing }, Cmd.none)

        focusOrUpdateModel newInputs input selCmd =
          if focus then
            ( { same | inputs = newInputs }, selCmd )
          else
            updateModelFromInput newInputs ctrl input
      in
        Dict.get ctrl.name inputs |>
        Maybe.map processFocusSelect |>
        Maybe.map
          (\(input, selCmd) ->
            focusOrUpdateModel (Dict.insert ctrl.name input inputs) input selCmd
          ) |>
        Maybe.withDefault ( same, Cmd.none )

    onSelect ctrl toSelmsg selMsg = -- SelectMsg
      let
        input = Dict.get ctrl.name inputs
      in
        input |>
        Maybe.andThen .select |>
        Maybe.map2
          (\selinp sel ->
            Select.update toSelmsg selMsg sel |>
            Tuple.mapFirst (\sm -> { selinp | select = Just sm })
          )
          input |>
        Maybe.map
          (Tuple.mapFirst
            (\selinp -> { same | inputs = Dict.insert ctrl.name selinp inputs })
          ) |>
        Maybe.withDefault ( same, Cmd.none )

    updateInputsFromModel newModel newInputs =
      Dict.foldl
        (\key _ foldedinps ->
          Dict.get key controllers |>
          Maybe.map
            (\(Controller ctrl) ->
              updateInput ctrl (ctrl.formatter newModel) foldedinps |>
              (\(ninps, _) -> ninps)
            ) |>
          Maybe.withDefault foldedinps
        )
        newInputs
        newInputs

    updateModel doInputUpdate transform (newModel, cmd) =
      { same | model = newModel } |>
      transform cmd |>
      (\newem ->
        if doInputUpdate && cmd == Cmd.none then -- update inputs
          newem.initializer newModel |>
          Maybe.map
            (\(ctrls, is) ->
              Dict.merge
                (\k i (r, me) -> (Dict.insert k i r, me)) -- input present only in initialized dict
                (\k i1 i2 (r, me) -> -- input present in both dicts, preserve editing flag and errors
                  ( Dict.insert k { i1 | editing = i2.editing, error = i2.error } r
                  , if i2.editing then Just k else me
                  )
                )
                (\_ _ (r, me) -> (r, me)) -- input present only in existing dict, drop it
                is
                newem.inputs
                (Dict.empty, Nothing) |>
              Tuple.mapBoth
                (\nis -> { newem | controllers = ctrls, inputs = nis })
                (\me ->
                  me |>
                  Maybe.andThen
                    (\e ->
                      Dict.get e ctrls |>
                      Maybe.map (\c -> do toMsg <| FocusNoSearchMsg c) -- restore select box if present
                    ) |>
                  Maybe.withDefault Cmd.none
                )
            ) |>
          Maybe.withDefault
            ( updateInputsFromModel (JM.data newem.model) newem.inputs |>
              (\ninps -> ( { newem | inputs = ninps }, Cmd.none ))
            )
        else (newem, cmd)
      )
  in
    case msg of
      -- JM model messages
      UpdateModelMsg doInputUpdate value ->
        JM.update (toMsg << UpdateModelMsg doInputUpdate) value model |>
        updateModel doInputUpdate (\_ nm -> nm)

      FetchModelMsg value ->
        JM.update (toMsg << FetchModelMsg) value model |>
        updateModel True (\_ nm -> nm) |>
        Tuple.mapFirst (\nm -> { nm | isDirty = False })

      SaveModelMsg value ->
        JM.update (toMsg << SaveModelMsg) value model |>
        updateModel True (\c nm -> { nm | isSaving = c /= Cmd.none }) |>
        Tuple.mapFirst (\nm -> { nm | isDirty = False })

      CreateModelMsg createFun value ->
        JM.update (toMsg << CreateModelMsg createFun) value model |>
        (\(nm, cmd) ->
          ( { same | model = nm }
          , if cmd == Cmd.none then set toMsg createFun else cmd
          )
        )

      DeleteModelMsg value ->
        JM.update (toMsg << DeleteModelMsg) value model |>
        updateModel True (\c nm -> { nm | isDeleting = c /= Cmd.none }) |>
        Tuple.mapFirst (\nm -> { nm | isDirty = False })

      -- Select messages
      SelectMsg (Controller ctrl) selMsg -> -- field select list messages
        onSelect ctrl (toMsg << SelectMsg (Controller ctrl)) selMsg

      -- user input messages
      OnMsg (Controller ctrl) value ->
        onInput (toMsg << SelectMsg (Controller ctrl)) ctrl value

      OnFocusMsg (Controller ctrl) focus ->
        setEditing ctrl focus True

      FocusNoSearchMsg (Controller ctrl) ->
        setEditing ctrl True False

      OnSelectMsg (Controller ctrl) value -> -- text selected from select component
        updateInput ctrl value inputs |>
        (\(newInputs, maybeInp) ->
          maybeInp |>
          Maybe.map
            (updateModelFromInput newInputs ctrl) |>
          Maybe.withDefault ( same, Cmd.none )
        )

      OnSelectFieldMsg name value ->
        ( same
        , Dict.get name controllers |>
          Maybe.map
            (\ctrl -> do toMsg <| OnSelectMsg ctrl value) |>
          Maybe.withDefault Cmd.none
        )

      ValidatePathMsg path ->
        (same, controllers
        |> Dict.get path
        |> Maybe.map (\c -> domsg <| toMsg <| ValidateFieldMsg c Nothing)
        |> Maybe.withDefault Cmd.none)

      ValidateFieldMsg (Controller ctrl) Nothing ->
        Dict.get ctrl.name inputs |>
        Maybe.map
          (\input ->
            case ctrl.validateInput input (JM.data model) of
              ValidationResult res ->
                ( updateValidationResults ctrl.name res, Cmd.none )

              ValidationTask task ->
                ( same
                , Task.attempt
                    (toMsg << ValidateFieldMsg (Controller ctrl) << Just << Tuple.pair input.value)
                    task
                )
          ) |>
        Maybe.withDefault (same, Cmd.none)

      ValidateFieldMsg (Controller {name}) (Just (value, res)) ->
        Dict.get name inputs |>
        Utils.filter (\input -> input.value == value) |>
        Maybe.map
          (\_ ->
            case res of
              Ok r ->
                ( updateValidationResults name r, Cmd.none )

              Err err ->
                ( same, Ask.error toMessagemsg err)
          ) |>
        Maybe.withDefault ( same, Cmd.none )


      ValidateAllMsg ->
        let
          mapResult ctrl = 
            List.map (\(name, err) ->
              if String.isEmpty name then
                (ctrl.name, err)
              else
                (name, err)
            )

          taskList =
            Dict.toList controllers
              |> List.concatMap (\(_, Controller ctrl) ->
                Dict.get ctrl.name inputs
                  |> Maybe.map (\input ->
                    case ctrl.validateInput input (JM.data model) of
                      ValidationResult res ->
                        [ Task.succeed (mapResult ctrl res) ]

                      ValidationTask task ->
                        [ Task.map (\res -> (mapResult ctrl res)) task ]
                  )
                  |> Maybe.withDefault []
              ) 
              |> Task.sequence
        in
        (same
        , Task.attempt
            (toMsg << ValidateAllUpdateMsg)
            taskList
        )

      ValidateAllUpdateMsg res ->
        let
          updateInputs resultList =
            Dict.map (\key input ->
              Dict.get key resultList
                |> Maybe.map (\err -> { input | error = if String.isEmpty err then Nothing else Just err })
                |> Maybe.withDefault input
                  
            ) inputs

          updatedInputs resultList = 
            resultList
              |> List.concat
              |> Dict.fromList
              |> updateInputs
        in
        case res of
          Ok resultList ->
            ( { same | inputs = updatedInputs resultList }
            , Cmd.none  
            )

          Err err ->
            ( same, Ask.error toMessagemsg err)

      UpdateFieldMsg (Controller ctrl) res ->
        ( { same |
            inputs =
              Dict.update ctrl.name (Maybe.map (\i -> { i | resolving = False })) inputs
          }
        , case res of
            Ok (val, updater) ->
              do toMsg <|
                CmdChainMsg
                  [ EditModelMsg (updater val), ValidateFieldMsg (Controller ctrl) Nothing ]
                  Cmd.none
                  Nothing

            Err err ->
              Ask.error toMessagemsg err
        )

      --edit entire model
      EditModelMsg editFun ->
        ( same, JM.set (toMsg << UpdateModelMsg True) <| editFun <| JM.data model )

      NewModelMsg searchParams createFun ->
        ( same, JM.create (toMsg << CreateModelMsg createFun) searchParams )

      SyncModelMsg ->
        updateModelFromActiveInput inputs

      SubmitModelMsg ->
        ( same
        , inputs |>
          Dict.values |>
          List.filter .resolving |>
          List.map .name |>
          (\rl ->
            if List.isEmpty rl then
              JM.save (toMsg << SaveModelMsg) []
            else
              Ask.error
                toMessagemsg
                ( "Cannot save data while resolving field value " ++
                  String.join ", " rl ++
                  ". Try later."
                )
          )
        )

      CmdChainMsg msgs cmd mmsg ->
        mmsg |>
        Maybe.map
          (\modmsg ->
              update (toMsg << CmdChainMsg msgs cmd << Just) modmsg same |>
              Tuple.mapSecond
                (\updcmd ->
                  if updcmd == Cmd.none then
                    do toMsg <| CmdChainMsg msgs cmd Nothing
                  else
                    updcmd
                )
          ) |>
        Maybe.withDefault
          ( same
          , case msgs of
              [] ->
                cmd

              modmsg :: rest ->
                do (toMsg << CmdChainMsg rest cmd << Just) modmsg
          )
