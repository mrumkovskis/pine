module EditModel exposing
  ( Input, Controller, ModelUpdater, InputValidator, Formatter, SelectInitializer
  , EditModel, JsonEditModel, JsonEditMsg, Msg, Tomsg, JsonController, JsonControllerInitializer
  , Msgs, SelectMsgs, ValidationResult (..)
  , init, initJsonForm, initJsonQueryForm
  , jsonController, jsonModelUpdater, jsonInputValidator, jsonFormatter
  , jsonFieldFormatter, jsonFieldParser, jsonSelectInitializer, jsonInputCmd
  , setModelUpdater, setFormatter, setSelectInitializer, setInputValidator
  , fetch, set, setMsg, create, createMsg, http, httpWithSetter, save, saveMsg, sync, syncMsg, delete
  , id, data, inp, inps, inpsByPattern, inpsTableByPattern
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

import Http
import Dict exposing (..)
import Json.Encode as JE
import Json.Decode as JD
import Task exposing (..)

import Debug exposing (log)


{-| Represents form input field. Is synchronized with model. -}
type alias Input msg =
  { name: String
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
  JM.FormModel msg model -> Maybe (Dict String (Controller msg model), Dict String (Input msg))


{-| Updates model from `Input` optionally emiting command.
    NOTE: Avoid using batch command since it may cause multiple save call!
-}
type alias ModelUpdater msg model = Tomsg msg model -> Input msg -> model -> (model, Cmd msg)


type alias JsonModelUpdater msg =
  ModelUpdater msg JM.JsonValue ->
  Dict String VM.View ->
  Tomsg msg JM.JsonValue ->
  Input msg ->
  JM.JsonValue ->
  (JM.JsonValue, Cmd msg)


type ValidationResult
  = ValidationResult (Result String String)
  | ValidationTask (Task String String)


{-| Validates input -}
type alias InputValidator model = String -> model -> ValidationResult


type alias JsonInputValidator = InputValidator JM.JsonValue -> String -> JM.JsonValue -> ValidationResult


{- Get input field text from model. Function is called when value is selected from list or model
   data are refreshed from update or fetch messages. Bool argument depends on EditModel.isEditable field
   so data can be formatted according to display mode.
-}
type alias Formatter model = model -> String


type alias JsonFormatter = Formatter JM.JsonValue -> JM.JsonValue -> String


type alias SelectInitializer msg model =
  Select.Tomsg msg JM.JsonValue -> -- toSelectmsg
  Ask.Tomsg msg -> -- toMessagemsg
  String -> -- search string
  (String -> msg) -> -- select msg
  model ->
  (SelectModel msg JM.JsonValue, Cmd msg)


{-| Controller. Binds [`Input`](#Input) together with [JsonModel](JsonModel) -}
type Controller msg model =
  Controller
    { name: String
    , updateModel: ModelUpdater msg model -- called on OnSelect, OnFocus _ False
    , formatter: Formatter model
    , selectInitializer: Maybe (SelectInitializer msg model) -- called on OnFocus _ True
    , validateInput: InputValidator model -- called on OnMsg, OnSelect
    , inputCmd: Maybe (String -> Cmd msg)
    }


type alias JsonController msg =
  { updateModel: Maybe (JsonModelUpdater msg)
  , formatter: Maybe (JsonFormatter)
  , validateInput: Maybe JsonInputValidator
  , selectInitializer: Maybe (SelectInitializer msg JM.JsonValue)
  , inputCmd: Maybe (String -> Cmd msg)
  , fieldFormatter: Maybe (JM.JsonValue -> String) -- unlike formatter takes field value as an argument not entire model
  , fieldParser: Maybe (Input msg -> Input msg) -- prepares input value for updater, for example parses date
  }


type alias JsonControllerInitializer msg =
  String -> VM.Field -> Maybe (JsonController msg)


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
type alias EditModel msg model =
  { model: JM.FormModel msg model
  , initializer: ModelInitializer msg model
  , controllers: Dict String (Controller msg model)
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


type alias JsonEditModel msg = EditModel msg JM.JsonValue


type alias JsonEditMsg msg = Msg msg JM.JsonValue


{-| Edit model update messages -}
type Msg msg model
  = UpdateModelMsg Bool (JM.FormMsg msg model)
  | FetchModelMsg (JM.FormMsg msg model)
  | SaveModelMsg (JM.FormMsg msg model)
  | CreateModelMsg (model -> model) (JM.FormMsg msg model)
  | DeleteModelMsg (JM.FormMsg msg model)
  -- select components messages
  | SelectMsg (Controller msg model) (Select.Msg msg JM.JsonValue)
  -- input fields event messages
  | OnMsg (Controller msg model) String
  | OnFocusMsg (Controller msg model) Bool
  | FocusNoSearchMsg (Controller msg model)
  | OnSelectMsg (Controller msg model) String
  | OnSelectFieldMsg String String
  | OnResolvedMsg String
  | ValidateFieldMsg String String model (Result String String)
  -- update entire model
  | EditModelMsg (model -> model)
  | SetModelMsg model
  | NewModelMsg JM.SearchParams (model -> model)
  | HttpModelMsg (Result HttpError model -> Maybe (model -> model)) (Result HttpError model)
  | SyncModelMsg
  | SubmitModelMsg
  --
  | CmdChainMsg (List (Msg msg model)) (Cmd msg) (Maybe (Msg msg model))


{-| Edit model message constructor -}
type alias Tomsg msg model = (Msg msg model -> msg)


{-| Initializes model
-}
init: JM.FormModel msg model -> List (String, Controller msg model) -> Ask.Tomsg msg -> EditModel msg model
init model ctrlList toMessagemsg =
  let
    controllers =
      ctrlList |>
      List.map (\(k, Controller c) -> (k, Controller { c | name = k })) |>
      Dict.fromList

    inputs =
      controllers |>
      Dict.map
        (\k _ ->
          Input k "" False Nothing Nothing Nothing -1 Nothing False False
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
  String -> String -> Maybe (JsonControllerInitializer msg) -> String ->
  List (String, JsonController msg) ->
  Ask.Tomsg msg -> JsonEditModel msg
initJsonForm =
  initJsonFormInternal .fields


initJsonQueryForm:
  String -> String -> Maybe (JsonControllerInitializer msg) -> String ->
  List (String, JsonController msg) ->
  Ask.Tomsg msg -> JsonEditModel msg
initJsonQueryForm =
  initJsonFormInternal .filter


initJsonFormInternal:
  (VM.View -> List VM.Field) -> String -> String -> Maybe (JsonControllerInitializer msg) ->
  String -> List (String, JsonController msg) ->
  Ask.Tomsg msg -> JsonEditModel msg
initJsonFormInternal fieldGetter metadataBaseUri dataBaseUri maybeInitializer typeName controllers toMessagemsg =
  let
    jsonFormInitializer (JM.Model _ { metadata } as formModel) =
      JM.flattenJsonForm fieldGetter formModel |>
      List.indexedMap
        (\i (path, field, value) ->
          let
            key =
              case path of
                JM.Name p JM.End ->
                  p

                JM.Idx idx JM.End ->
                  String.fromInt i

                _ ->
                  JM.pathEncoder path |> JE.encode 0

            maybeExtensions =
              case List.filter (\(p, _) -> JM.pathMatch p key) controllers of
                [] ->
                  maybeInitializer |>
                  Maybe.andThen (\ctlInit -> ctlInit key field)

                (_, match) :: rest -> -- TODO prioritize matched controllers if more than one match is found
                  Just match

            fieldFormatter =
              maybeExtensions |>
              Maybe.andThen .fieldFormatter |>
              Maybe.withDefault JM.jsonValueToString

            fieldParser =
              maybeExtensions |>
              Maybe.andThen .fieldParser |>
              Maybe.withDefault identity


            ctrl =
              let
                updater toMsg cinp model =
                  let
                    maybeUpdateSubList val =
                      JM.jsonReader path model |>
                      Maybe.map
                        (\exv -> case exv of
                          JM.JsList vals ->
                            Utils.find ((==) val) vals |>
                            Maybe.map (\_ -> model) |>
                            Maybe.withDefault
                              (JM.jsonEditor (JM.appendPath path <| JM.EndIdx JM.End) val model)

                          _ -> JM.jsonEditor (JM.appendPath path JM.End) val model
                        )
                  in
                    if field.isComplexType then
                      let
                        subviewDecoder _ =
                          JM.jsonDecoder |>
                          JD.map
                            (\val ->
                              if field.isCollection then
                                -- set value if value not exists already
                                maybeUpdateSubList val |>
                                Maybe.withDefault model
                              else
                                JM.jsonEditor path val model
                            )

                        updateSingleStringFieldSubview initmod =
                          Dict.get field.typeName metadata |>
                          Maybe.map .fields |>
                          Maybe.map (List.filter (\f -> f.jsonType == "string")) |>
                          Maybe.andThen
                            (\fs -> case fs of
                              [ f ] ->
                                maybeUpdateSubList <|
                                  JM.JsObject <|
                                    Dict.fromList
                                      [(f.name, JM.JsString <| (fieldParser cinp).value)]

                              _ -> Nothing
                            ) |>
                          Maybe.withDefault initmod
                      in
                        ( model
                        , httpWithSetter
                            toMsg
                            ( dataBaseUri ++ "/create/" ++ field.typeName ++
                              Utils.httpQuery [(field.name, cinp.value)]
                            )
                            (subviewDecoder ())
                            (Result.toMaybe >> Maybe.map always >> Utils.orElse (Just updateSingleStringFieldSubview))
                        )
                    else if field.isCollection then
                      ( ( fieldParser >> .value >> JM.stringToJsonValue field.jsonType ) cinp |>
                        Maybe.andThen maybeUpdateSubList |>
                        Maybe.withDefault model
                      , Cmd.none
                      )
                    else
                      ( fieldParser >> .value >> JM.stringToJsonValue field.jsonType ) cinp |>
                      Maybe.map (\iv -> ( JM.jsonEditor path iv model, Cmd.none )) |>
                      Maybe.withDefault ( model, Cmd.none )

                formatter model =
                  JM.jsonReader path model |>
                  Maybe.map fieldFormatter |>
                  Maybe.withDefault ""

                validator iv mod =
                  let
                    typeValidator t =
                      case t of
                        "number" ->
                          let
                            res = Maybe.map (\_ -> iv) >> Result.fromMaybe ("Not a number: " ++ iv)
                          in
                            field.fractionDigits |>
                            Maybe.map
                              (\fd ->
                                if fd > 0 then String.toFloat iv |> res else String.toInt iv |> res
                              ) |>
                            Maybe.withDefault (String.toInt iv |> res)

                        "boolean" ->
                          String.toLower iv |>
                            (\s ->
                              if s == "true" || s == "false" then
                                Ok iv
                              else
                                Err <| "Not a boolean: " ++ iv
                            )

                        _ -> Ok iv

                    enumValidator en =
                      if String.isEmpty iv then
                        Ok iv
                      else
                        Utils.find ((==) iv) en |>
                        Result.fromMaybe ("Value must come from list")
                  in
                    typeValidator field.jsonType |>
                    Result.andThen
                      (\r ->
                        field.enum |>
                        Maybe.map enumValidator |>
                        Maybe.withDefault (Ok r)
                      ) |>
                    ValidationResult
              in
                maybeExtensions |>
                Maybe.map
                  (\ext ->
                    Controller
                      { name = key
                      , updateModel =
                          ext.updateModel |>
                          Maybe.map (\u -> u updater metadata) |>
                          Maybe.withDefault updater
                      , formatter =
                          ext.formatter |>
                          Maybe.map (\f -> f formatter) |>
                          Maybe.withDefault formatter
                      , selectInitializer = ext.selectInitializer
                      , validateInput =
                          ext.validateInput |>
                          Maybe.map (\v -> v validator) |>
                          Maybe.withDefault validator
                      , inputCmd = ext.inputCmd
                      }
                  ) |>
                Maybe.withDefault
                  ( Controller
                      { name = key
                      , updateModel = updater
                      , formatter = formatter
                      , selectInitializer = Nothing
                      , validateInput = validator
                      , inputCmd = Nothing
                      }
                  )
          in
            ( ( key, ctrl )
            , ( key
              , Input key (fieldFormatter value) False Nothing Nothing Nothing i (Just field) False False
              )
            )
        ) |>
        List.unzip |>
        Tuple.mapBoth Dict.fromList Dict.fromList
  in
    EditModel
      (JM.initJsonValueForm fieldGetter metadataBaseUri dataBaseUri typeName toMessagemsg)
      (jsonFormInitializer >> Just)
      Dict.empty
      Dict.empty
      toMessagemsg
      False
      False
      True
      False


jsonController: JsonController msg
jsonController =
  JsonController Nothing Nothing Nothing Nothing Nothing Nothing Nothing


jsonModelUpdater: JsonModelUpdater msg -> JsonController msg -> JsonController msg
jsonModelUpdater updater jc =
  { jc | updateModel = Just updater }


jsonInputValidator: JsonInputValidator -> JsonController msg -> JsonController msg
jsonInputValidator validator jc =
  { jc | validateInput = Just validator }


jsonFormatter: JsonFormatter -> JsonController msg -> JsonController msg
jsonFormatter formatter jc =
  { jc | formatter = Just formatter }


jsonFieldFormatter: (JM.JsonValue -> String) -> JsonController msg -> JsonController msg
jsonFieldFormatter formatter jc =
  { jc | fieldFormatter = Just formatter }


jsonFieldParser: (Input msg -> Input msg) -> JsonController msg -> JsonController msg
jsonFieldParser parser jc =
  { jc | fieldParser = Just parser }


jsonSelectInitializer: SelectInitializer msg JM.JsonValue -> JsonController msg -> JsonController msg
jsonSelectInitializer initializer jc =
  { jc | selectInitializer = Just initializer }


jsonInputCmd: Maybe (String -> Cmd msg) -> JsonController msg -> JsonController msg
jsonInputCmd maybeInputCmd jc =
  { jc | inputCmd = maybeInputCmd }


setModelUpdater: String -> ModelUpdater msg model -> EditModel msg model -> EditModel msg model
setModelUpdater key updater model =
  updateController key (\(Controller c) -> Controller { c | updateModel = updater }) model


setFormatter: String -> Formatter model -> EditModel msg model -> EditModel msg model
setFormatter key formatter model =
  updateController key (\(Controller c) -> Controller { c | formatter = formatter }) model


setSelectInitializer: String -> Maybe (SelectInitializer msg model) -> EditModel msg model -> EditModel msg model
setSelectInitializer key initializer model =
  updateController
    key
    (\(Controller c) -> Controller { c | selectInitializer = initializer }) model


setInputValidator: String -> InputValidator model -> EditModel msg model -> EditModel msg model
setInputValidator key validator model =
  updateController key (\(Controller c) -> Controller { c | validateInput = validator }) model


updateController: String -> (Controller msg model -> Controller msg model) -> EditModel msg model -> EditModel msg model
updateController key updater model =
  Dict.get key model.controllers |>
  Maybe.map updater |>
  Maybe.map (\c -> Dict.insert key c model.controllers) |>
  Maybe.map (\cs -> { model | controllers = cs}) |>
  Maybe.withDefault model


{-| Fetch data by id from server. Calls [`JsonModel.fetch`](JsonModel#fetch)
-}
fetch: Tomsg msg model -> Int -> Cmd msg
fetch toMsg fid =
  JM.fetch (toMsg << FetchModelMsg) <| [ ("id", String.fromInt fid) ]


fetchMsg: Tomsg msg model -> Int -> msg
fetchMsg toMsg fid =
  JM.fetchMsg (toMsg << FetchModelMsg) <| [ ("id", String.fromInt fid) ]


{-| Set model data. After updating inputs, calls [`JsonModel.set`](JsonModel#set)
-}
set: Tomsg msg model -> (model -> model) -> Cmd msg
set toMsg editFun =
  domsg <| setMsg toMsg editFun


setMsg: Tomsg msg model -> (model -> model) -> msg
setMsg toMsg editFun =
  toMsg <| EditModelMsg editFun


{-| Creates model data, calling [`JsonModel.set`](JsonModel#create).
After that call function `createFun` on received data.
-}
create: Tomsg msg model -> JM.SearchParams -> (model -> model) -> Cmd msg
create toMsg createParams createFun =
  domsg <| createMsg toMsg createParams createFun


createMsg: Tomsg msg model -> JM.SearchParams -> (model -> model) -> msg
createMsg toMsg createParams createFun =
  toMsg <| NewModelMsg createParams createFun


{-| Creates model from http request. Setter function's returns optional model update function
(see [`set`](#set)) which gets existing model as an argument thus the result can be a product from
both new and existing model.
If Result is Ok and setter function returns Nothing, model remains unchanged, if Result is Err
and setter function returns Nothing, http error message is propagated to Ask module.
-}
httpWithSetter: Tomsg msg model -> String -> JD.Decoder model -> (Result HttpError model -> Maybe (model -> model)) -> Cmd msg
httpWithSetter toMsg url decoder setter =
  Http.get { url = url, expect = expectJson (toMsg << HttpModelMsg setter) decoder }


http: Tomsg msg model -> String -> JD.Decoder model -> Cmd msg
http toMsg url decoder =
  httpWithSetter toMsg url decoder (Result.toMaybe >> Maybe.map always)


{-| Save model to server.  Calls [`JsonModel.save`](JsonModel#save)
-}
save: Tomsg msg model -> Cmd msg
save =
  domsg << saveMsg


saveMsg: Tomsg msg model -> msg
saveMsg toMsg =
  toMsg SubmitModelMsg


sync: Tomsg msg model -> Cmd msg
sync =
  domsg << syncMsg


syncMsg: Tomsg msg model -> msg
syncMsg toMsg =
  toMsg <| SyncModelMsg


{-| Save model from server.  Calls [`JsonModel.delete`](JsonModel#delete)
-}
delete: Tomsg msg model -> Int -> Cmd msg
delete toMsg did =
  JM.delete (toMsg << DeleteModelMsg) [("id", String.fromInt did)]


{-| Gets model id.  Calls [`JsonModel.id`](JsonModel#id) and tries to convert result to `Int`
-}
id: EditModel msg model -> Maybe Int
id =
  .model >> JM.id >> Maybe.andThen String.toInt


{-| Gets model data.
-}
data: EditModel msg model -> model
data { model } =
  JM.data model


{-| Creates simple controller -}
simpleCtrl: (String -> model -> model) -> Formatter model -> Controller msg model
simpleCtrl updateModel formatter =
  Controller
    { name = ""
    , updateModel = noCmdUpdater updateModel
    , formatter = formatter
    , selectInitializer = Nothing
    , validateInput = \_ _ -> ValidationResult <| Ok ""
    , inputCmd = Nothing
    }


{-| Creates simple controller with select list -}
simpleSelectCtrl: (String -> model -> model) -> Formatter model -> SelectInitializer msg model -> Controller msg model
simpleSelectCtrl updateModel formatter selectInitializer =
  Controller
    { name = ""
    , updateModel = noCmdUpdater updateModel
    , formatter = formatter
    , selectInitializer = Just selectInitializer
    , validateInput = \_ _ -> ValidationResult <| Ok ""
    , inputCmd = Nothing
    }


noCmdUpdater: (String -> model -> model) -> ModelUpdater msg model
noCmdUpdater updater =
  \_ input mod -> (updater input.value mod, Cmd.none)


{-| Creates controller -}
controller:
  ModelUpdater msg model ->
  Formatter model ->
  Maybe (SelectInitializer msg model) ->
  InputValidator model ->
  Maybe (String -> Cmd msg) ->
  Controller msg model
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
inp: String -> Tomsg msg model -> EditModel msg model -> Maybe (Input msg)
inp key toMsg { controllers, inputs } =
  Maybe.map2 (inpInternal toMsg)
    (Dict.get key controllers)
    (Dict.get key inputs)


{-| Gets inputs from model for rendering
-}
inps: List String -> Tomsg msg model -> EditModel msg model -> List (Input msg)
inps keys toMsg model =
  List.foldl
    (\k r -> inp k toMsg model |> Maybe.map (\i -> i :: r) |> Maybe.withDefault r)
    []
    keys |>
  List.reverse


inpsByPattern: String -> Tomsg msg model -> EditModel msg model -> List (Input msg)
inpsByPattern pattern toMsg { controllers, inputs } =
  Dict.filter (\k _ -> JM.pathMatch pattern k) inputs |>
  Dict.values |>
  List.sortBy .idx |>
  List.concatMap
    (\i ->
      Dict.get i.name controllers |>
      Maybe.map (\c -> inpInternal toMsg c i) |>
      Maybe.map List.singleton |>
      Maybe.withDefault []
    )


inpsTableByPattern: Tomsg msg model -> List String -> EditModel msg model -> List (List (Input msg))
inpsTableByPattern toMsg patterns model =
  patterns |>
  List.map (\p -> inpsByPattern p toMsg model) |>
  Utils.transpose


inpInternal: Tomsg msg model -> Controller msg model -> Input msg -> Input msg
inpInternal toMsg ctl input =
  let
    onEnter = syncMsg toMsg

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
                      (\isActive ->
                        if isActive then
                          navigationMsg toSMsg
                        else
                          (\ev ->
                            case ev of
                              SE.Select -> onEnter

                              x -> navigationMsg toSMsg x
                          )
                      )
                  , setActivemsg = setActiveMsg toSMsg
                  , selectmsg =  selectMsg toSMsg
                  }
            )
      }
  in
    { input | msgs = Just msgs }


{-| Produces `OnMsg` input message. This can be used to set or clear text in input field.
-}
onInputMsg: String -> Tomsg msg model -> EditModel msg model -> Maybe (String -> msg)
onInputMsg key toMsg { controllers } =
  Dict.get key controllers |>
  Maybe.map (\ctrl -> toMsg << OnMsg ctrl)


{-| Produces `OnSelectMsg` input message which updates model from input.
    And then executes command. This function can be used to set controller's `inputCmd`
-}
onInputCmd: String -> Tomsg msg model -> Cmd msg -> String -> Cmd msg
onInputCmd key toMsg cmd value =
  do toMsg <| CmdChainMsg [ OnSelectFieldMsg key value ] cmd Nothing


{-| Produces `OnSelectMsg` input message which updates model from input.
  This can be used on input events like `onCheck` or `onClick` to update model from input
-}
inputMsg: String -> Tomsg msg model -> EditModel msg model -> Maybe (String -> msg)
inputMsg key toMsg { controllers } =
  Dict.get key controllers |>
  Maybe.map (\ctrl -> toMsg << OnSelectMsg ctrl)


{-| Produces `EditModelMsg` message given json encoded `Path`
-}
jsonEditMsg: Tomsg msg JM.JsonValue -> String -> JM.JsonValue -> msg
jsonEditMsg toMsg path value =
  toMsg <| EditModelMsg (\m -> JM.jsonEdit path value m)


{-| Produces `EditModelMsg` message given json encoded `Path` with `JsonValue` `JsNull`
-}
jsonDeleteMsg: Tomsg msg JM.JsonValue -> String -> msg
jsonDeleteMsg toMsg path =
  toMsg <| EditModelMsg (\m -> JM.jsonEdit path JM.JsNull m)


{-| Model update -}
update: Tomsg msg model -> Msg msg model -> EditModel msg model -> (EditModel msg model, Cmd msg)
update toMsg msg ({ model, inputs, controllers, toMessagemsg } as same) =
  let
    updateModelFromInput newInputs ctrl input =
      let
        mod = JM.data model
      in
        if ctrl.formatter mod == input.value then
          ( { same | inputs = newInputs }, Cmd.none )
        else
          ctrl.updateModel
            (toMsg << CmdChainMsg [ OnResolvedMsg input.name ] Cmd.none << Just) -- unset resolving flag after cmd
            input
            mod |>
          (\(nm, cmd) ->
            updateInputsFromModel nm newInputs |>
            (\(is, valTasks) ->
              ( { same |
                  inputs =
                    if cmd == Cmd.none then
                      is
                    else
                      is |>
                      Dict.update input.name (Maybe.map (\i -> { i | resolving = True})) -- set resolving flag if cmd
                , isDirty = True
                } --update inputs if updater has changed other fields
              , do toMsg <|
                  CmdChainMsg
                    [ SetModelMsg nm ]
                    ((if cmd == Cmd.none then valTasks else cmd :: valTasks) |> Cmd.batch)
                    Nothing
              )
            )
          )

    updateModelFromActiveInput =
      Dict.values >>
      List.filter .editing >>
      List.head >>
      Maybe.andThen
        (\input -> Dict.get input.name controllers |> Maybe.map (\(Controller c) -> (c, input))) >>
      Maybe.map (\(c, input) -> updateModelFromInput inputs c input) >>
      Maybe.withDefault (same, Cmd.none)

    updateInput ctrl value newInputs =
      Dict.get ctrl.name newInputs |>
      Maybe.map
        (\input ->
          case ctrl.validateInput value <| JM.data model of
            ValidationResult (Ok val) ->
              ( { input |
                  value = value
                , error = Nothing
                , select = input.select |> Maybe.map (Select.updateSearch value)
                }
              , Nothing
              )

            ValidationResult (Err err) ->
              ( { input |
                  value = value
                , error = Just err
                }
              , Nothing
              )

            ValidationTask t ->
              ( { input |
                  value = value
                , select = input.select |> Maybe.map (Select.updateSearch value)
                }
              , Just <|
                  Task.attempt (toMsg << ValidateFieldMsg ctrl.name value (JM.data model)) t
              )
        ) |>
      Maybe.map
        (\(input, mbt) -> (Dict.insert ctrl.name input newInputs, Just input, mbt)) |>
      Maybe.withDefault (newInputs, Nothing, Nothing)

    onInput toSelectmsg ctrl value = -- OnMsg
      updateInput ctrl value inputs |>
      (\(ninps, mbinp, mbvalt) ->
        ( { same | inputs = ninps }
        , mbinp |> Maybe.andThen .select |>
          Maybe.map (\_ -> [ Select.search toSelectmsg value ]) |> -- select search command
          Maybe.withDefault [] |>
          (\cmds -> mbvalt |> Maybe.map (\valt -> valt :: cmds) |> Maybe.withDefault cmds) |> -- validation task
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
          if focus then
            ctrl.selectInitializer |>
            Maybe.map
              (\initializer ->
                initializer
                  (toMsg << SelectMsg (Controller ctrl))
                  same.toMessagemsg
                  input.value
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
        (\key input (foldedinps, valtasks) ->
          Dict.get key controllers |>
          Maybe.map
            (\(Controller ctrl) ->
              updateInput ctrl (ctrl.formatter newModel) foldedinps |>
              (\(ninps, _, mbvalt) ->
                ( ninps
                , Maybe.map (\valt -> valt :: valtasks) mbvalt |> Maybe.withDefault valtasks
                )
              )
            ) |>
          Maybe.withDefault (foldedinps, valtasks)
        )
        ( newInputs, [] )
        newInputs

    updateModel doInputUpdate transform (newModel, cmd) =
      { same | model = newModel } |>
      transform cmd |>
      (\newem ->
        if doInputUpdate && cmd == Cmd.none then -- update inputs
          newem.initializer newModel |>
          Maybe.map
            (\(ctrls, is) ->
              ( { newem | controllers = ctrls, inputs = is }
              , newem.inputs |>
                Dict.filter (\_ i -> i.editing ) |>
                Dict.values |>
                List.head |>
                Maybe.andThen (\i -> Dict.get i.name ctrls) |>
                Maybe.map (\c -> do toMsg <| FocusNoSearchMsg c) |> -- restore select box if present
                Maybe.withDefault Cmd.none
              )
            ) |>
          Maybe.withDefault
            ( updateInputsFromModel (JM.data newem.model) newem.inputs |>
              (\(ninps, valtasks) -> ( { newem | inputs = ninps }, Cmd.batch valtasks ))
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
        (\(newInputs, maybeInp, _) -> -- do not execute validation task since it will be run by updateModelFromInput function
          maybeInp |>
          Maybe.map (updateModelFromInput newInputs ctrl) |>
          Maybe.withDefault ( same, Cmd.none )
        )

      OnSelectFieldMsg name value ->
        ( same
        , Dict.get name controllers |>
          Maybe.map
            (\ctrl -> do toMsg <| OnSelectMsg ctrl value) |>
          Maybe.withDefault Cmd.none
        )

      OnResolvedMsg name ->
        ( { same |
            inputs =
              inputs |>
              Dict.update name (Maybe.map (\i -> { i | resolving = False }))
          }
        , Cmd.none
        )

      ValidateFieldMsg name value mod res ->
        Dict.get name inputs |>
        Utils.filter (\input -> input.value == value && JM.data model == mod) |>
        Maybe.map
          (\input ->
            case res of
              Ok _ ->
                { input | error = Nothing }

              Err err ->
                { input | error = Just err }
          ) |>
        Maybe.map
          (\input ->
            ( { same | inputs = Dict.insert name input inputs }, Cmd.none )
          ) |>
        Maybe.withDefault ( same, Cmd.none )

      --edit entire model
      EditModelMsg editFun ->
        ( same, JM.set (toMsg << UpdateModelMsg True) <| editFun <| JM.data model )

      SetModelMsg mod ->
        ( same, JM.set (toMsg << UpdateModelMsg False) mod )

      NewModelMsg searchParams createFun ->
        ( same, JM.create (toMsg << CreateModelMsg createFun) searchParams )

      HttpModelMsg setter httpResult ->
        let
          mod = JM.data model

          result =
            case httpResult of
              Ok _ ->
                setter httpResult |>
                Maybe.map (set toMsg) |>
                Maybe.withDefault Cmd.none

              Err e ->
                setter httpResult |>
                Maybe.map (set toMsg) |>
                Maybe.withDefault (Ask.errorOrUnauthorized same.toMessagemsg e)
        in
          ( same, result )

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
