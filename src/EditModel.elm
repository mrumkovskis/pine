module EditModel exposing
  ( Input, TextInput, AddressInput
  , Controller, TextController, AddressController
  , InputWithAttributes, TextInputWithAttributes, AddressInputWithAttributes
  , EditModel, Msg, Tomsg
  , fetch, set, create, save, delete, id
  , inputEvents, onTextSelectInput, onTextSelectMouse
  , addressInputEvents, onAddressSelectInput, onAddressSelectMouse
  , update
  )

{-| Binding layer between [`Input`](#Input) representing form input field and [`JsonModel`](JsonModel).
[Controller](#Controller) functions are responsible for binding.

# Commands
@docs fetch, set, create, save, delete

# Inuput attributes (input is associated with controller)
@docs inputEvents, onTextSelectInput, onTextSelectMouse,
      addressInputEvents, onAddressSelectInput, onAddressSelectMouse

# Utility
@docs id

# Types
@docs AddressController, AddressInput, AddressInputWithAttributes, Controller, EditModel,
      Input, InputWithAttributes, Msg, TextController, TextInput, TextInputWithAttributes,
      Tomsg

@docs update
-}


import JsonModel as JM
import Ask
import DeferredRequests as DR
import Select exposing (..)
import AddressModel exposing (..)
import Utils

import Html exposing (Attribute)
import Html.Events exposing (..)
import Task

import Debug exposing (log)


{-| Represents form input field. Is synchronized with model. -}
type alias Input msg value =
  { input: String
  , editing: Bool
  , error: Maybe String
  , select: Maybe (SelectModel msg value)
  }


{-| Text input field. -}
type alias TextInput msg = Input msg String


{-| Address input field. -}
type alias AddressInput msg = Input msg Address


{- Get field value from input field text -}
type alias Getter value = String -> Result String value


{- Set field value to model. Method ir called while user is typing in an input field.
   Bool argument is True if value is selected like from Select component with
   Enter key or mouse, False otherwise.
-}
type alias Setter value model = Bool -> value -> model -> Result String model


{- Get input field text from model. Method is called when value is selected from list or model
   data are refreshed from update or fetch messages. Bool argument depends on EditModel.isEditable field
   so data can be formatted according to display mode.
-}
type alias Formatter model = Bool -> model -> String


type alias InputUpdater msg value inputs = inputs -> Input msg value -> inputs


type alias InputGetter msg value inputs = inputs -> Input msg value


{- Updates inputs from model. Method is called when model is updated from update or fetch messages.
   In general calls controllers formatter methods and updates inputs.
   Bool argument depends on EditModel.isEditable field so data can be formatted according to display mode.
-}
type alias InputsUpdater controllers model inputs = Bool -> controllers -> model -> inputs


type alias SelectInitializer msg value =
  Ask.Tomsg msg ->
  DR.Tomsg msg ->
  String ->
  (value -> msg) ->
  SelectModel msg value


{-| Controller. Binds [`Input`](#Input) together with [JsonModel](JsonModel) -}
type alias Controller msg value model inputs =
  { value: Getter value
  , setter: Setter value model
  , formatter: Formatter model
  , guiUpdater: InputUpdater msg value inputs
  , guiGetter: InputGetter msg value inputs
  , selectInitializer: Maybe (SelectInitializer msg value)
  }


{-| Text field controller -}
type alias TextController msg model inputs = Controller msg String model inputs


{-| Address field controller -}
type alias AddressController msg model inputs = Controller msg Address model inputs


{-| Input together with proposed html input element attributes and with
mouse selection attributes of select component.
-}
type alias InputWithAttributes msg value =
  { input: Input msg value
  , mouseSelectAttrs: Int -> List (Attribute msg)
  , attrs: List (Attribute msg)
  }


{-| Text onput and attributes -}
type alias TextInputWithAttributes msg = InputWithAttributes msg String


{-| Address input and attributes -}
type alias AddressInputWithAttributes msg = InputWithAttributes msg Address



{-| Edit model -}
type alias EditModel msg model inputs controllers =
  { model: JM.FormModel msg model
  , inputs: inputs
  , controllers: controllers
  , inputsUpdater: InputsUpdater controllers model inputs
  , toMessagemsg: Ask.Tomsg msg
  , toDeferredmsg: DR.Tomsg msg
  , isSaving: Bool
  , isDeleting: Bool
  , isEditable: Bool
  }


{-| Edit model update messages -}
type Msg msg model inputs
  = UpdateModelMsg Bool (JM.FormMsg msg model)
  | FetchModelMsg (JM.FormMsg msg model)
  | SaveModelMsg (JM.FormMsg msg model)
  | CreateModelMsg (model -> model) (JM.FormMsg msg model)
  | DeleteModelMsg (JM.FormMsg msg model)
  -- select components messages
  | SelectTextMsg (TextController msg model inputs) (Select.Msg msg String)
  | SelectAddressMsg (AddressController msg model inputs) (Select.Msg msg Address)
  -- input fields event messages
  | OnMsg (TextController msg model inputs) String
  | OnFocusMsg (TextController msg model inputs) Bool
  | OnAddressMsg (AddressController msg model inputs) String
  | OnAddressFocusMsg (AddressController msg model inputs) Bool
  | OnTextSelect (TextController msg model inputs) String
  | OnAddressSelect (AddressController msg model inputs) Address
  -- update entire model
  | EditModelMsg (model -> model)
  | NewModelMsg JM.SearchParams (model -> model)


{-| Edit model message constructor -}
type alias Tomsg msg model inputs = (Msg msg model inputs -> msg)


{-| Fetch data by id from server. Calls [`JsonModel.fetch`](JsonModel#fetch)
-}
fetch: Tomsg msg model inputs -> Int -> Cmd msg
fetch toMsg fid =
  JM.fetch (toMsg << FetchModelMsg) <| [ ("id", String.fromInt fid) ]


{-| Set model data. After updating inputs, calls [`JsonModel.set`](JsonModel#set)
-}
set: Tomsg msg model inputs -> (model -> model) -> Cmd msg
set toMsg editFun =
  Task.perform toMsg <| Task.succeed <| EditModelMsg editFun


{-| Creates model data, calling [`JsonModel.set`](JsonModel#create).
After that call function `createFun` on received data.
-}
create: Tomsg msg model inputs -> JM.SearchParams -> (model -> model) -> Cmd msg
create toMsg createParams createFun =
  Task.perform toMsg <| Task.succeed <| NewModelMsg createParams createFun


{-| Save model to server.  Calls [`JsonModel.save`](JsonModel#save)
-}
save: Tomsg msg model inputs -> Cmd msg
save toMsg =
  JM.save (toMsg << SaveModelMsg) []


{-| Save model from server.  Calls [`JsonModel.delete`](JsonModel#delete)
-}
delete: Tomsg msg model inputs -> Int -> Cmd msg
delete toMsg did =
  JM.delete (toMsg << DeleteModelMsg) [("id", String.fromInt did)]


{-| Gets model id.  Calls [`JsonModel.id`](JsonModel#id) and tries to convert result to `Int`
-}
id: EditModel msg model inputs controllers -> Maybe Int
id =
  .model >> JM.id >> Maybe.andThen String.toInt


{- event attributes private function -}
inputFocusBlurEvents:
  Tomsg msg model inputs ->
  (String -> Msg msg model inputs) ->
  Msg msg model inputs ->
  Msg msg model inputs ->
  List (Attribute msg)
inputFocusBlurEvents toMsg inputMsg focusMsg blurMsg =
  [ onInput <| toMsg << inputMsg
  , onFocus <| toMsg focusMsg
  , onBlur <| toMsg blurMsg
  ]


{- Select event listeners -}

{-| Returns `onInput`, `onFocus`, `onBlur` `Html.Attributes`
for input associated with the controller.
-}
inputEvents: Tomsg msg model inputs -> TextController msg model inputs -> List (Attribute msg)
inputEvents toMsg ctrl =
  inputFocusBlurEvents
    toMsg
    (OnMsg ctrl)
    (OnFocusMsg ctrl True)
    (OnFocusMsg ctrl False)


{-| Returns attributes for [`Select`](Select) management. Generally this is key listener
reacting on arrow, escape, enter keys.
-}
onTextSelectInput: Tomsg msg model inputs -> TextController msg model inputs -> List (Attribute msg)
onTextSelectInput toMsg ctrl =
  Select.onSelectInput <| toMsg << SelectTextMsg ctrl


{-| Returns attributes for [`Select`](Select) management. Generally this is mouse down listener
to enable value selection from list. `Int` parameter indicates selected index.
-}
onTextSelectMouse: Tomsg msg model inputs -> TextController msg model inputs -> Int -> List (Attribute msg)
onTextSelectMouse toMsg ctrl idx =
  Select.onMouseSelect (toMsg << SelectTextMsg ctrl) idx


{-| The same as [`inputEvents`](#inputEvents) with the difference that instead of string value
[`AddressModel.Address`](AddressModel#Address) is selected.
-}
addressInputEvents: Tomsg msg model inputs -> AddressController msg model inputs -> List (Attribute msg)
addressInputEvents toMsg ctrl =
  inputFocusBlurEvents
    toMsg
    (OnAddressMsg ctrl)
    (OnAddressFocusMsg ctrl True)
    (OnAddressFocusMsg ctrl False)


{-| The same as [`onTextSelectInput`](#onTextSelectInput) with the difference that instead of string value
[`AddressModel.Address`](AddressModel#Address) is selected.
-}
onAddressSelectInput: Tomsg msg model inputs -> AddressController msg model inputs -> List (Attribute msg)
onAddressSelectInput toMsg ctrl =
  Select.onSelectInput <| toMsg << SelectAddressMsg ctrl


{-| The same as [`onTextSelectMouse`](#onTextSelectMouse) with the difference that instead of string value
[`AddressModel.Address`](AddressModel#Address) is selected.
-}
onAddressSelectMouse: Tomsg msg model inputs -> AddressController msg model inputs -> Int -> List (Attribute msg)
onAddressSelectMouse toMsg ctrl idx =
  Select.onMouseSelect (toMsg << SelectAddressMsg ctrl) idx

-- end of select event listeners


{-| Model update -}
update: Tomsg msg model inputs -> Msg msg model inputs -> EditModel msg model inputs controllers -> (EditModel msg model inputs controllers, Cmd msg)
update toMsg msg ({ model, inputs, controllers } as same) =
  let
    apply ctrl input modelValueResult =
      let fieldGui = ctrl.guiGetter inputs in
      case modelValueResult of
        Ok value ->
          ( { same |
              inputs = ctrl.guiUpdater inputs { fieldGui | input = input, error = Nothing }
            }
          , [ JM.set (toMsg << UpdateModelMsg False) value ]
          )

        Err err ->
          ( { same |
              inputs = ctrl.guiUpdater inputs { fieldGui | input = input, error = Just err }
            }
          , []
          )

    applyInput toSelectmsg ctrl value =
      let
        res =
          apply ctrl value <|
            ( ctrl.value value |>
              Result.andThen
                (Utils.flip (ctrl.setter False) <| (JM.data model))
            )

        searchCmd =
          (ctrl.guiGetter inputs).select |>
          Maybe.map (always <| Select.search toSelectmsg value) |>
          Maybe.map List.singleton |>
          Maybe.withDefault []
      in
        res |>
        Tuple.mapSecond (\cmds -> Cmd.batch (cmds ++ searchCmd))

    applySelectedValue ctrl value modelData =
      let
        newDataRes = ctrl.setter True value modelData

        formatter = ctrl.formatter same.isEditable

        input = newDataRes |> Result.map formatter |> Result.withDefault "<error setting value!>"
      in
        apply ctrl input newDataRes |>
        Tuple.mapSecond Cmd.batch

    initSelectBase ctrl initializer =
      initializer
        same.toMessagemsg
        same.toDeferredmsg
        ((ctrl.guiGetter >> .input) inputs)

    initTextSelect ctrl initializer =
      (initSelectBase ctrl initializer)
        (toMsg << OnTextSelect ctrl)

    initAddressSelect ctrl initializer =
      (initSelectBase ctrl initializer)
        (toMsg << OnAddressSelect ctrl)

    setEditing ctrl initializer focus =
      let
        newModel =
          let fieldGui = ctrl.guiGetter inputs in
            { same | inputs = ctrl.guiUpdater inputs { fieldGui | editing = focus } }

        select =
          if focus then
            ctrl.selectInitializer |>
              Maybe.map (initializer ctrl)
          else Nothing
      in
        Tuple.pair
          ( updateSelect
              ctrl
              newModel
              select
          )
          Cmd.none

    updateSelect ctrl newModel value =
      let
        fieldGui = ctrl.guiGetter newModel.inputs
      in
        { newModel | inputs = ctrl.guiUpdater newModel.inputs { fieldGui | select = value } }

    applySelect ctrl newModel toSelmsg selMsg =
      (ctrl.guiGetter newModel.inputs).select |>
      Maybe.map (Select.update toSelmsg selMsg) |>
      Maybe.map (Tuple.mapFirst Just) |>
      Maybe.map (Tuple.mapFirst (updateSelect ctrl newModel)) |>
      Maybe.withDefault (Tuple.pair newModel Cmd.none)

    -- this method is expected to be called from gui so inputs are not updated
    updateInputs newModel =
      same.inputsUpdater same.isEditable controllers <| JM.data newModel

    updateModel doInputUpdate newModel =
      { same |
        model = newModel
      , inputs = if doInputUpdate then updateInputs newModel else same.inputs
      }

    applyCreateModel newModel =
      { same | model = newModel }

    createCmd createFun cmd =
      if cmd == Cmd.none then set toMsg createFun else cmd

    applyModel newModel =
      { same |
        model = newModel
      , inputs = updateInputs newModel
      }

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
        case JM.update (toMsg << UpdateModelMsg doInputUpdate) data model of
          (newModel, cmd) ->
            (updateModel doInputUpdate newModel, cmd)

      FetchModelMsg data ->
        case JM.update (toMsg << FetchModelMsg) data model of
          (newModel, cmd) ->
            (applyFetchModel newModel, cmd)

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
      SelectTextMsg ctrl selMsg -> -- address field select list messages
        applySelect ctrl same (toMsg << SelectTextMsg ctrl) selMsg

      SelectAddressMsg ctrl selMsg -> -- address field select list messages
        applySelect ctrl same (toMsg << SelectAddressMsg ctrl) selMsg

      -- user input messages
      OnMsg ctrl value ->
        applyInput (toMsg << SelectTextMsg ctrl) ctrl value

      OnFocusMsg ctrl focus ->
        setEditing ctrl initTextSelect focus

      OnAddressMsg ctrl value -> -- address field input
        applyInput (toMsg << SelectAddressMsg ctrl) ctrl value

      OnAddressFocusMsg ctrl focus ->
        setEditing ctrl initAddressSelect focus

      OnTextSelect ctrl value -> -- text selected from select component
        (applySelectedValue ctrl value <| JM.data model)

      OnAddressSelect ctrl address -> -- address selected from select component
        (applySelectedValue ctrl address <| JM.data model)

      --edit entire model
      EditModelMsg editFun ->
        ( same, JM.set (toMsg << UpdateModelMsg True) <| editFun <| JM.data model )

      NewModelMsg searchParams createFun ->
        ( same, JM.create (toMsg << CreateModelMsg createFun) searchParams )
