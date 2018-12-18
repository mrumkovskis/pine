module EditModel exposing
  ( Input, Controller, Attributes, ModelUpdater, InputValidator, Formatter, SelectInitializer
  , InputAttrs, EditModel, Msg, Tomsg
  , init, setModelUpdater, setFormatter, setSelectInitializer, setInputValidator
  , fetch, set, create, http, save, delete
  , basicInpAttrs, basicTaAttrs, inpAttrs, inpAttrsWithMouse, inpKey
  , inputEvents, onSelectInput, inputSelectEvents, onSelectMouse
  , id, inp, inps, noCmd, simpleController, controller
  , update
  )

{-| Binding layer between [`Input`](#Input) representing form input field and [`JsonModel`](JsonModel).
[Controller](#Controller) functions are responsible for binding.

# Commands
@docs fetch, set, create, http, save, delete

# Inuput attributes (input is associated with controller)
@docs inputEvents, onSelectInput, onSelectMouse

# Utility
@docs id, noCmd, input

# Types
@docs Controller, EditModel, Input, InputWithAttributes, Msg, Tomsg

@docs update
-}


import JsonModel as JM
import Ask
import Select exposing (..)
import Utils

import Html exposing (Attribute)
import Html.Attributes as Attrs
import Html.Events exposing (..)
import Task
import Http
import Dict exposing (..)

import Debug exposing (toString, log)


{-| Represents form input field. Is synchronized with model. -}
type alias Input msg =
  { value: String
  , editing: Bool
  , error: Maybe String
  , select: Maybe (SelectModel msg String)
  , attrs: Attributes msg
  }


{-| Updates model from `Input` optionally emiting command. -}
type alias ModelUpdater msg model = Tomsg msg model -> Input msg -> model -> (model, Cmd msg)


{-| Validates input -}
type alias InputValidator = String -> Result String String


{- Get input field text from model. Function is called when value is selected from list or model
   data are refreshed from update or fetch messages. Bool argument depends on EditModel.isEditable field
   so data can be formatted according to display mode.
-}
type alias Formatter model = model -> String


type alias SelectInitializer msg =
  Ask.Tomsg msg ->
  String ->
  (String -> msg) ->
  SelectModel msg String


{-| First element of tuple is static attributes, second - input event attributes, the value
of which can be obtained form functions like `inputEvents` or `onSelectInput`,
third - value selection from list with mouse click event attributes, the value of which
can be obtained from function like `onSelectMouse`. -}
type alias InputAttrs msg model =
  ( List (Attribute msg)
  , (Controller msg model -> List (Attribute msg))
  , (Controller msg model -> Int -> List (Attribute msg))
  )


{-| Controller. Binds [`Input`](#Input) together with [JsonModel](JsonModel) -}
type Controller msg model =
  Controller
    { name: String
    , updateModel: ModelUpdater msg model -- called on OnSelect, OnFocus _ False
    , formatter: Formatter model
    , selectInitializer: Maybe (SelectInitializer msg) -- called on OnFocus _ True
    , validateInput: InputValidator -- called on OnMsg, OnSelect
    }


{-| Input together with proposed html input element attributes and with
mouse selection attributes of select component.
-}
type alias Attributes msg =
  { mouseSelectAttrs: Int -> List (Attribute msg)
  , attrs: List (Attribute msg)
  }


{-| Edit model -}
type alias EditModel msg model =
  { model: JM.FormModel msg model
  , controllers: Dict String (Controller msg model)
  , inputs: Dict String (Input msg)
  , toMessagemsg: Ask.Tomsg msg
  --, validate: Tomsg msg model -> model -> (Result String model, Cmd msg)
  --, error: Maybe String
  , isSaving: Bool
  , isDeleting: Bool
  --, isValidating: Bool
  , isEditable: Bool
  }


{-| Edit model update messages -}
type Msg msg model
  = UpdateModelMsg Bool (JM.FormMsg msg model)
  | FetchModelMsg (JM.FormMsg msg model)
  | SaveModelMsg (JM.FormMsg msg model)
  | CreateModelMsg (model -> model) (JM.FormMsg msg model)
  | DeleteModelMsg (JM.FormMsg msg model)
  -- select components messages
  | SelectMsg (Controller msg model) (Select.Msg msg String)
  -- input fields event messages
  | OnMsg (Controller msg model) String
  | OnFocusMsg (Controller msg model) Bool
  | OnSelectMsg (Controller msg model) String
  -- update entire model
  | EditModelMsg (model -> model)
  | NewModelMsg JM.SearchParams (model -> model)
  | HttpModelMsg (Maybe model) (Result Http.Error model)


{-| Edit model message constructor -}
type alias Tomsg msg model = (Msg msg model -> msg)


{-| Initializes model
-}
init: JM.FormModel msg model -> List (key, Controller msg model) -> Ask.Tomsg msg  -> EditModel msg model
init model ctrlList toMessagemsg =
  let
    controllers =
      ctrlList |>
      List.map (\(k, Controller c) -> (toString k, Controller { c | name = toString k })) |>
      Dict.fromList

    emptyAttrs =
      Attributes (always []) []

    inputs =
      controllers |>
      Dict.map (\k _ -> Input "" False Nothing Nothing emptyAttrs)
  in
    EditModel
      model
      controllers
      inputs
      toMessagemsg
      False
      False
      True


setModelUpdater: key -> ModelUpdater msg model -> EditModel msg model -> EditModel msg model
setModelUpdater key updater model =
  updateController key (\(Controller c) -> Controller { c | updateModel = updater }) model


setFormatter: key -> Formatter model -> EditModel msg model -> EditModel msg model
setFormatter key formatter model =
  updateController key (\(Controller c) -> Controller { c | formatter = formatter }) model


setSelectInitializer: key -> Maybe (SelectInitializer msg) -> EditModel msg model -> EditModel msg model
setSelectInitializer key initializer model =
  updateController
    key
    (\(Controller c) -> Controller { c | selectInitializer = initializer }) model


setInputValidator: key -> InputValidator -> EditModel msg model -> EditModel msg model
setInputValidator key validator model =
  updateController key (\(Controller c) -> Controller { c | validateInput = validator }) model


updateController: key -> (Controller msg model -> Controller msg model) -> EditModel msg model -> EditModel msg model
updateController key updater model =
  Dict.get (toString key) model.controllers |>
  Maybe.map updater |>
  Maybe.map (\c -> Dict.insert (toString key) c model.controllers) |>
  Maybe.map (\cs -> { model | controllers = cs}) |>
  Maybe.withDefault model


{-| Fetch data by id from server. Calls [`JsonModel.fetch`](JsonModel#fetch)
-}
fetch: Tomsg msg model -> Int -> Cmd msg
fetch toMsg fid =
  JM.fetch (toMsg << FetchModelMsg) <| [ ("id", String.fromInt fid) ]


{-| Set model data. After updating inputs, calls [`JsonModel.set`](JsonModel#set)
-}
set: Tomsg msg model -> (model -> model) -> Cmd msg
set toMsg editFun =
  Task.perform toMsg <| Task.succeed <| EditModelMsg editFun


{-| Creates model data, calling [`JsonModel.set`](JsonModel#create).
After that call function `createFun` on received data.
-}
create: Tomsg msg model -> JM.SearchParams -> (model -> model) -> Cmd msg
create toMsg createParams createFun =
  Task.perform toMsg <| Task.succeed <| NewModelMsg createParams createFun


{-| Creates model from http request.
-}
http: Tomsg msg model -> Maybe model -> Http.Request model -> Cmd msg
http toMsg maybeOnErr req =
  Http.send (toMsg << HttpModelMsg maybeOnErr) req


{-| Save model to server.  Calls [`JsonModel.save`](JsonModel#save)
-}
save: Tomsg msg model -> Cmd msg
save toMsg =
  JM.save (toMsg << SaveModelMsg) []


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


{-| Utility function which helps to create model updater returning `Cmd.none` -}
noCmd: (String -> model -> model) -> Tomsg msg model -> Input msg -> model -> (model, Cmd msg)
noCmd simpleSetter _ input model =
  ( simpleSetter input.value model, Cmd.none )


{-| Creates simple controller -}
simpleController: ModelUpdater msg model -> Formatter model -> Controller msg model
simpleController updateModel formatter =
  Controller
    { name = ""
    , updateModel = updateModel
    , formatter = formatter
    , selectInitializer = Nothing
    , validateInput = Ok
    }


{-| Creates controller -}
controller:
  ModelUpdater msg model ->
  Formatter model ->
  Maybe (SelectInitializer msg) ->
  InputValidator ->
  Controller msg model
controller updateModel formatter selectInitializer validator =
  Controller
    { name = ""
    , updateModel = updateModel
    , formatter = formatter
    , selectInitializer = selectInitializer
    , validateInput = validator
    }


{-| Gets input from model for rendering
-}
inp: key -> InputAttrs msg model -> EditModel msg model -> Maybe (Input msg)
inp key (staticAttrs, inputAttrs, mouseSelectAttrs) { controllers, inputs } =
  let
    ks = toString key
  in
    Dict.get ks controllers |>
    Maybe.map2
      (\input ctl ->
        let
          attrs =
            (Attrs.value input.value :: staticAttrs) ++ inputAttrs ctl

          mouseAttrs =
            mouseSelectAttrs ctl
        in
          { input | attrs = Attributes mouseAttrs attrs }
      )
      (Dict.get ks inputs)


{-| Gets inputs from model for rendering
-}
inps: List (key, InputAttrs msg model) -> EditModel msg model -> List (Input msg)
inps keys model =
  List.foldl
    (\(k, ia) r -> inp k ia model |> Maybe.map (\i -> i :: r) |> Maybe.withDefault r)
    []
    keys |>
  List.reverse


basicInpAttrs: String -> String -> Int -> List (Attribute msg)
basicInpAttrs type_ placeholder size =
  [ Attrs.type_ type_, Attrs.placeholder placeholder, Attrs.size size ]


basicTaAttrs: Int -> Int -> List (Attribute msg)
basicTaAttrs rows cols =
  [ Attrs.rows rows, Attrs.cols cols ]


inpAttrs:
  List (Attribute msg) ->
  (Controller msg model -> List (Attribute msg)) ->
  InputAttrs msg model
inpAttrs attrs inpEvents =
  inpAttrsWithMouse
    attrs inpEvents (\_ _ -> [])


inpAttrsWithMouse:
  List (Attribute msg) ->
  (Controller msg model -> List (Attribute msg)) ->
  (Controller msg model -> Int -> List (Attribute msg)) ->
  InputAttrs msg model
inpAttrsWithMouse attrs inpEvents mouseSelectEvents =
  (attrs, inpEvents, mouseSelectEvents)


inpKey: key -> InputAttrs msg model -> (key, InputAttrs msg model)
inpKey key attrs =
  ( key, attrs )


{- event attributes private function -}
inputFocusBlurEvents:
  Tomsg msg model ->
  (String -> Msg msg model) ->
  Msg msg model ->
  Msg msg model ->
  List (Attribute msg)
inputFocusBlurEvents toMsg inputMsg focusMsg blurMsg =
  [ onInput <| toMsg << inputMsg
  , onFocus <| toMsg focusMsg
  , onBlur <| toMsg blurMsg
  ]


{-| Returns `onInput`, `onFocus`, `onBlur` `Html.Attributes`
for input associated with the controller.
-}
inputEvents: Tomsg msg model -> Controller msg model -> List (Attribute msg)
inputEvents toMsg ctrl =
  inputFocusBlurEvents
    toMsg
    (OnMsg ctrl)
    (OnFocusMsg ctrl True)
    (OnFocusMsg ctrl False)


{-| Returns attributes for [`Select`](Select) management. Generally this is key listener
reacting on arrow, escape, enter keys.
-}
onSelectInput: Tomsg msg model -> Controller msg model -> List (Attribute msg)
onSelectInput toMsg ctrl =
  Select.onSelectInput <| toMsg << SelectMsg ctrl


{-| Returns concatenation of `inputEvents` ++ `onSelectInput` -}
inputSelectEvents: Tomsg msg model -> Controller msg model -> List (Attribute msg)
inputSelectEvents toMsg ctrl =
  inputEvents toMsg ctrl ++ onSelectInput toMsg ctrl


{-| Returns attributes for [`Select`](Select) management. Generally this is mouse down listener
to enable value selection from list. `Int` parameter indicates selected index.
-}
onSelectMouse: Tomsg msg model -> Controller msg model -> Int -> List (Attribute msg)
onSelectMouse toMsg ctrl idx =
  Select.onMouseSelect (toMsg << SelectMsg ctrl) idx


{-| Model update -}
update: Tomsg msg model -> Msg msg model -> EditModel msg model -> (EditModel msg model, Cmd msg)
update toMsg msg ({ model, inputs, controllers } as same) =
  let
    updateModelFromInputs newInputs =
      Dict.foldl
        (\key input ((mod, cmds), newInps) ->
          Dict.get key controllers |>
          Maybe.map
            (\(Controller ctrl) ->
              if ctrl.formatter mod == input.value then
                ((mod, cmds), newInps)
              else
                (ctrl.updateModel toMsg input mod) |>
                Tuple.mapSecond (\cmd -> if cmd == Cmd.none then cmds else cmd :: cmds) |>
                (\(m, c) ->
                  ( (m, c)
                    {- if after updating model formatter returns different value do input update
                       so that input is synchronized with model -}
                    , let v = ctrl.formatter m in
                      if v /= input.value then updateInput ctrl v newInps else newInps
                  )
                )
            ) |>
          Maybe.withDefault ((mod, cmds), newInps)
        )
        ( ( JM.data model, [] ), newInputs )
        newInputs |>
      Tuple.mapFirst (Tuple.mapSecond (List.reverse >> Cmd.batch)) |>
      (\((nm, cmd), ni) ->
        ( { same | inputs = ni }
        , if cmd == Cmd.none then JM.set (toMsg << UpdateModelMsg False) nm else cmd
        )
      )

    updateInput ctrl value newInputs =
      Dict.get ctrl.name newInputs |>
      Maybe.map
        (\input ->
          case ctrl.validateInput value of
            Ok val ->
              { input |
                value = value
              , error = Nothing
              , select = input.select |> Maybe.map (Select.updateSearch value)
              }

            Err err ->
              { input | value = value, error = Just err }
        ) |>
      Maybe.map (\input -> Dict.insert ctrl.name input newInputs) |>
      Maybe.withDefault newInputs

    applyInput toSelectmsg ctrl value = -- OnMsg
      let
        newInputs = updateInput ctrl value inputs

        searchCmd =
          Dict.get ctrl.name inputs |>
          Maybe.andThen .select |>
          Maybe.map (always <| Select.search toSelectmsg value) |>
          Maybe.withDefault Cmd.none
      in
        ( { same | inputs = newInputs }, searchCmd )

    setEditing ctrl focus =  -- OnFocus
      let
        select value =
          if focus then
            ctrl.selectInitializer |>
            Maybe.map
              (\initializer ->
                initializer
                  same.toMessagemsg
                  value
                  (toMsg << OnSelectMsg (Controller ctrl))
              )
          else Nothing

        onEv input =
          { input | editing = focus, select = select input.value }

        maybeUpdateModel newInputs =
          if focus then
            ( { same | inputs = newInputs }, Cmd.none )
          else
            updateModelFromInputs newInputs
      in
        Dict.get ctrl.name inputs |>
        Maybe.map onEv |>
        Maybe.map (\input -> Dict.insert ctrl.name input inputs) |>
        Maybe.map maybeUpdateModel |>
        Maybe.withDefault ( same, Cmd.none )

    applySelect ctrl toSelmsg selMsg = -- SelectMsg
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

    updateInputsFromModel newModel =
      let
        data = JM.data newModel
      in
        Dict.foldl
          (\key input newInputs ->
            Dict.get key controllers |>
            Maybe.map
              (\(Controller ctrl) ->
                updateInput ctrl (ctrl.formatter data) newInputs
              ) |>
            Maybe.withDefault newInputs
          )
          inputs
          inputs

    updateModel doInputUpdate newModel =
      { same |
        model = newModel
      , inputs = if doInputUpdate then updateInputsFromModel newModel else same.inputs
      }

    applyCreateModel newModel =
      { same | model = newModel }

    createCmd createFun cmd =
      if cmd == Cmd.none then set toMsg createFun else cmd

    applyModel (newModel, cmd) =
      updateModel (cmd == Cmd.none) newModel

    applyFetchModel newModelAndCmd =
      applyModel newModelAndCmd

    applySaveModel newModelAndCmd =
      applyModel newModelAndCmd |>
      (\m -> { m | isSaving = Tuple.second newModelAndCmd /= Cmd.none })

    applyDeleteModel newModelAndCmd =
      applyModel newModelAndCmd |>
      (\m -> { m | isDeleting = Tuple.second newModelAndCmd /= Cmd.none })
  in
    case msg of
      -- JM model messages
      UpdateModelMsg doInputUpdate data ->
        JM.update (toMsg << UpdateModelMsg doInputUpdate) data model |>
        Tuple.mapFirst (updateModel doInputUpdate)

      FetchModelMsg data ->
        JM.update (toMsg << FetchModelMsg) data model |>
        (\mc -> (applyFetchModel mc, Tuple.second mc))

      SaveModelMsg data ->
        JM.update (toMsg << SaveModelMsg) data model |>
        (\mc -> (applySaveModel mc, Tuple.second mc))

      CreateModelMsg createFun data ->
        JM.update (toMsg << CreateModelMsg createFun) data model |>
        Tuple.mapBoth applyCreateModel (createCmd createFun)

      DeleteModelMsg data ->
        JM.update (toMsg << DeleteModelMsg) data model |>
        (\mc -> (applyDeleteModel mc, Tuple.second mc))

      -- Select messages
      SelectMsg (Controller ctrl) selMsg -> -- field select list messages
        applySelect ctrl (toMsg << SelectMsg (Controller ctrl)) selMsg

      -- user input messages
      OnMsg (Controller ctrl) value ->
        applyInput (toMsg << SelectMsg (Controller ctrl)) ctrl value

      OnFocusMsg (Controller ctrl) focus ->
        setEditing ctrl focus

      OnSelectMsg (Controller ctrl) value -> -- text selected from select component
        updateModelFromInputs <| updateInput ctrl value inputs

      --edit entire model
      EditModelMsg editFun ->
        ( same, JM.set (toMsg << UpdateModelMsg True) <| editFun <| JM.data model )

      NewModelMsg searchParams createFun ->
        ( same, JM.create (toMsg << CreateModelMsg createFun) searchParams )

      HttpModelMsg onErr httpResult ->
        let
            result =
              case httpResult of
                Ok r ->
                  set toMsg (always r)

                Err e ->
                  onErr |>
                  Maybe.map (\r -> set toMsg (always r)) |>
                  Maybe.withDefault (Ask.errorOrUnauthorized same.toMessagemsg e)
        in
          ( same, result )
