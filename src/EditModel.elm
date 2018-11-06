module EditModel exposing
  ( Input, Controller, Attributes, ModelUpdater, AttrsGetter, InputValidator
  , EditModel, Msg, Tomsg
  , init, fetch, set, create, http, save, delete
  , inputEvents, onSelectInput, onSelectMouse
  , id, input, attrs, noCmd, simpleController, controller
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
import DeferredRequests as DR
import Select exposing (..)
import Utils

import Html exposing (Attribute)
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
  }


{-| Updates model from `Input` optionally emiting command. -}
type alias ModelUpdater msg model = Tomsg msg model -> Input msg -> model -> (model, Cmd msg)


{-| Get input element attributes from controller -}
type alias AttrsGetter msg model = Tomsg msg model -> Controller msg model -> Attributes msg


{-| Validates input -}
type alias InputValidator = String -> Result String String


{- Get input field text from model. Function is called when value is selected from list or model
   data are refreshed from update or fetch messages. Bool argument depends on EditModel.isEditable field
   so data can be formatted according to display mode.
-}
type alias Formatter model = model -> String


type alias SelectInitializer msg =
  Ask.Tomsg msg ->
  DR.Tomsg msg ->
  String ->
  (String -> msg) ->
  SelectModel msg String


{-| Controller. Binds [`Input`](#Input) together with [JsonModel](JsonModel) -}
type Controller msg model =
  Controller
    { name: String
    , updateModel: ModelUpdater msg model -- called on OnSelect, OnFocus _ False
    , formatter: Formatter model
    , attrs: AttrsGetter msg model
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
  , toDeferredmsg: DR.Tomsg msg
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
  | HttpModelMsg (Result Http.Error model)


{-| Edit model message constructor -}
type alias Tomsg msg model = (Msg msg model -> msg)


{-| Initializes model
-}
init: JM.FormModel msg model -> List (key, Controller msg model) -> Ask.Tomsg msg -> DR.Tomsg msg -> EditModel msg model
init model ctrlList toMessagemsg toDeferredmsg =
  let
    controllers =
      ctrlList |>
      List.map (\(k, Controller c) -> (toString k, Controller { c | name = toString k })) |>
      Dict.fromList

    inputs =
      controllers |>
      Dict.map (\k _ -> Input "" False Nothing Nothing)
  in
    EditModel
      model
      controllers
      inputs
      toMessagemsg
      toDeferredmsg
      False
      False
      True


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
http: Tomsg msg model -> Http.Request model -> Cmd msg
http toMsg req =
  Http.send (toMsg << HttpModelMsg) req


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
noCmd simpleSetter _ inp model =
  ( simpleSetter inp.value model, Cmd.none )


{-| Creates simple controller -}
simpleController: ModelUpdater msg model -> Formatter model -> AttrsGetter msg model -> Controller msg model
simpleController updateModel formatter attrGetter =
  Controller
    { name = ""
    , updateModel = updateModel
    , formatter = formatter
    , attrs = attrGetter
    , selectInitializer = Nothing
    , validateInput = Ok
    }


{-| Creates controller -}
controller:
  ModelUpdater msg model ->
  Formatter model ->
  AttrsGetter msg model ->
  Maybe (SelectInitializer msg) ->
  InputValidator ->
  Controller msg model
controller updateModel formatter attrGetter selectInitializer validator =
  Controller
    { name = ""
    , updateModel = updateModel
    , formatter = formatter
    , attrs = attrGetter
    , selectInitializer = selectInitializer
    , validateInput = validator
    }


{-| Gets attributes from model -}
attrs: Tomsg msg model -> key -> EditModel msg model -> Attributes msg
attrs toMsg key model =
  Dict.get (toString key) model.controllers |>
  Maybe.map (\(Controller ctrl) -> ctrl.attrs toMsg (Controller ctrl)) |>
  Maybe.withDefault (Attributes (always []) [])


{-| Gets input from model
-}
input: key -> EditModel msg model -> Maybe (Input msg)
input key { inputs } =
  Dict.get (toString key) inputs


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
        (\key inp (mod, cmds) ->
          Dict.get key controllers |>
          Maybe.map
            (\(Controller ctrl) ->
              if ctrl.formatter mod == inp.value then
                (mod, cmds)
              else
                (ctrl.updateModel toMsg inp mod) |>
                Tuple.mapSecond (\cmd -> if cmd == Cmd.none then cmds else cmd :: cmds)
            ) |>
          Maybe.withDefault (mod, cmds)
        )
        (JM.data model, [])
        newInputs |>
      Tuple.mapSecond List.reverse |>
      Tuple.mapSecond Cmd.batch |>
      (\(nm, cmd) ->
        ( { same | inputs = newInputs }
        , if cmd == Cmd.none then JM.set (toMsg << UpdateModelMsg False) nm else cmd
        )
      )

    updateInput ctrl value =
      Dict.get ctrl.name inputs |>
      Maybe.map
        (\inp ->
          case ctrl.validateInput value of
            Ok val ->
              { inp |
                value = value
              , error = Nothing
              , select = inp.select |> Maybe.map (Select.updateSearch value)
              }

            Err err ->
              { inp | value = value, error = Just err }
        ) |>
      Maybe.map (\inp -> Dict.insert ctrl.name inp inputs) |>
      Maybe.withDefault inputs

    applyInput toSelectmsg ctrl value = -- OnMsg
      let
        newInputs = updateInput ctrl value

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
                  same.toDeferredmsg
                  value
                  (toMsg << OnSelectMsg (Controller ctrl))
              )
          else Nothing

        onEv inp =
          { inp | editing = focus, select = select inp.value }

        maybeUpdateModel newInputs =
          if focus then
            ( { same | inputs = newInputs }, Cmd.none )
          else
            updateModelFromInputs newInputs
      in
        Dict.get ctrl.name inputs |>
        Maybe.map onEv |>
        Maybe.map (\inp -> Dict.insert ctrl.name inp inputs) |>
        Maybe.map maybeUpdateModel |>
        Maybe.withDefault ( same, Cmd.none )

    applySelect ctrl toSelmsg selMsg = -- SelectMsg
      let
        inp = Dict.get ctrl.name inputs
      in
        inp |>
        Maybe.andThen .select |>
        Maybe.map2
          (\selinp sel ->
            Select.update toSelmsg selMsg sel |>
            Tuple.mapFirst (\sm -> { selinp | select = Just sm })
          )
          inp |>
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
          (\key inp inps ->
            Dict.get key controllers |>
            Maybe.map
              (\(Controller ctrl) ->
                if ctrl.formatter data == inp.value then
                  inps
                else
                  Dict.insert key { inp | value = ctrl.formatter data } inps
              ) |>
            Maybe.withDefault inps
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

    applyModel newModel =
      updateModel False newModel

    applyFetchModel newModel =
      applyModel newModel

    applySaveModel isSaving newModel =
      applyModel newModel |> (\nm -> { nm | isSaving = isSaving })

    applyDeleteModel isDeleting newModel =
      applyModel newModel |> (\nm -> { nm | isDeleting = isDeleting })
  in
    case msg of
      -- JM model messages
      UpdateModelMsg doInputUpdate data ->
        JM.update (toMsg << UpdateModelMsg doInputUpdate) data model |>
        Tuple.mapFirst (updateModel doInputUpdate)

      FetchModelMsg data ->
        JM.update (toMsg << FetchModelMsg) data model |>
        Tuple.mapFirst applyFetchModel

      SaveModelMsg data ->
        case JM.update (toMsg << SaveModelMsg) data model of
          (newModel, cmd) ->
            (applySaveModel (not <| cmd == Cmd.none) newModel, cmd)

      CreateModelMsg createFun data ->
        JM.update (toMsg << CreateModelMsg createFun) data model |>
        Tuple.mapBoth applyCreateModel (createCmd createFun)

      DeleteModelMsg data ->
        case JM.update (toMsg << DeleteModelMsg) data model of
          (newModel, cmd) ->
            (applyDeleteModel (not <| cmd == Cmd.none) newModel, cmd)

      -- Select messages
      SelectMsg (Controller ctrl) selMsg -> -- field select list messages
        applySelect ctrl (toMsg << SelectMsg (Controller ctrl)) selMsg

      -- user input messages
      OnMsg (Controller ctrl) value ->
        applyInput (toMsg << SelectMsg (Controller ctrl)) ctrl value

      OnFocusMsg (Controller ctrl) focus ->
        setEditing ctrl focus

      OnSelectMsg (Controller ctrl) value -> -- text selected from select component
        updateModelFromInputs <| updateInput ctrl value

      --edit entire model
      EditModelMsg editFun ->
        ( same, JM.set (toMsg << UpdateModelMsg True) <| editFun <| JM.data model )

      NewModelMsg searchParams createFun ->
        ( same, JM.create (toMsg << CreateModelMsg createFun) searchParams )

      HttpModelMsg httpResult ->
        let
            result =
              case httpResult of
                Ok r ->
                  set toMsg (always r)

                Err e ->
                  Ask.error same.toMessagemsg <| Utils.httpErrorToString e
        in
          ( same, result )
