module EditModel exposing
  ( Input, Controller, InputWithAttributes
  , EditModel, Msg, Tomsg
  , fetch, set, create, http, save, delete, id, noCmd
  , inputEvents, onTextSelectInput, onTextSelectMouse
  , update
  )

{-| Binding layer between [`Input`](#Input) representing form input field and [`JsonModel`](JsonModel).
[Controller](#Controller) functions are responsible for binding.

# Commands
@docs fetch, set, create, http, save, delete

# Inuput attributes (input is associated with controller)
@docs inputEvents, onTextSelectInput, onTextSelectMouse,

# Utility
@docs id, noCmd

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

import Debug exposing (log)


{-| Represents form input field. Is synchronized with model. -}
type alias Input msg =
  { input: String
  , editing: Bool
  , error: Maybe String
  , select: Maybe (SelectModel msg String)
  }


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
    { validate: String -> Input msg
    , updateModel: Tomsg msg model -> Input msg -> model -> (model, Cmd msg) -- called on OnSelect, OnFocus _ False
    , formatter: model -> String  -- tiek izmantots veikstpējas nolūkos updatojot Inputus no modeļa, lai noteiktu vai ir jāsauc inputUpdater funka.
    , updateInput: model -> Input msg -- tiek izsaukts uz JsonModel messagiem, (varbūt optimizācijai kad cmd == Cmd.none?)
    , userInput: String -> Input msg -- tiek izsaukts uz OnMsg, OnSelect
    , selectInitializer: Maybe (SelectInitializer msg) -- tiek izsaukts uz OnFocus _ True
    , attrs: Tomsg msg model -> Controller msg model -> Attributes msg
    , name: String
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
  --, validate: Tomsg msg model controllers -> model -> (Result String model, Cmd msg)
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
  | OnSelect (Controller msg model) String
  -- update entire model
  | EditModelMsg (model -> model)
  | NewModelMsg JM.SearchParams (model -> model)
  | HttpModelMsg (Result Http.Error model)


{-| Edit model message constructor -}
type alias Tomsg msg model = (Msg msg model -> msg)


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


{-| Utility function which helps to create setter returning `Cmd.none` -}
noCmd: (String -> model -> Result String model) -> Tomsg msg model -> Bool -> String -> model -> ( Result String model, Cmd msg )
noCmd simpleSetter toMsg selected value model =
  ( simpleSetter value model, Cmd.none )


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


{- Select event listeners -}

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
onTextSelectInput: Tomsg msg model -> Controller msg model -> List (Attribute msg)
onTextSelectInput toMsg ctrl =
  Select.onSelectInput <| toMsg << SelectMsg ctrl


{-| Returns attributes for [`Select`](Select) management. Generally this is mouse down listener
to enable value selection from list. `Int` parameter indicates selected index.
-}
onTextSelectMouse: Tomsg msg model -> Controller msg model -> Int -> List (Attribute msg)
onTextSelectMouse toMsg ctrl idx =
  Select.onMouseSelect (toMsg << SelectMsg ctrl) idx


-- end of select event listeners

updateModel: Tomsg msg model -> Dict String (Input msg) -> Dict String (Controller msg model) -> model -> (model, Cmd msg)
updateModel toMsg inputs controllers model =
  Dict.foldl
    (\key inp (mod, cmds) ->
      Dict.get key controllers |>
      Maybe.map
        (\ctrl ->
          if ctrl.formatter model == inp.input then
            (mod, cmds)
          else
            (ctrl.updateModel toMsg inp model) |>
            Tuple.mapSecond (\cmd -> cmd :: cmds)
        ) |>
      Maybe.withDefault (mod, cmds)
    )
    (model, [])
    inputs |>
  Tuple.mapSecond List.reverse |>
  Tuple.mapSecond Cmd.batch



updateInputs: Dict String (Controller msg model) -> model -> Dict String (Input msg) -> Dict String (Input msg)
updateInputs controllers model inputs =
  Dict.foldl
    (\key inp inps ->
      Dict.get key controllers |>
      Maybe.map
        (\ctrl ->
          if ctrl.formatter model == inp.input then
            inputs
          else
            Dict.insert key (ctrl.updateInput model) inputs
        )
    )
    inputs
    inputs


{-| Model update -}
update: Tomsg msg model inputs -> Msg msg model inputs -> EditModel msg model inputs controllers -> (EditModel msg model inputs controllers, Cmd msg)
update toMsg msg ({ model, inputs, controllers } as same) =
  let
    apply ctrl input (modelValueResult, cmd) =
      let
        fieldGui = ctrl.guiGetter inputs

        updateSelectModel =
          fieldGui.select |> Maybe.map (Select.updateSearch input)
      in
        case modelValueResult of
          Ok value ->
            ( { same |
                inputs =
                  ctrl.guiUpdater
                    inputs
                    { fieldGui | input = input, error = Nothing, select = updateSelectModel }
              }
            , if cmd == Cmd.none then
                [ JM.set (toMsg << UpdateModelMsg False) value ]
              else [ cmd ]
            )

          Err err ->
            ( { same |
                inputs = ctrl.guiUpdater inputs { fieldGui | input = input, error = Just err }
              }
            , if cmd /= Cmd.none then [ cmd ] else []
            )

    applyInput toSelectmsg ctrl value =
      let
        resVal = ctrl.setter toMsg False value <| JM.data model

        searchCmd =
          (ctrl.guiGetter inputs).select |>
          Maybe.map (always <| Select.search toSelectmsg value) |>
          Maybe.map List.singleton |>
          Maybe.withDefault []
      in
        apply ctrl value resVal|>
        Tuple.mapSecond (\cmds -> Cmd.batch (cmds ++ searchCmd))

    applySelectedValue ctrl value modelData =
      let
        newDataRes = ctrl.setter toMsg True value modelData

        formatter = ctrl.formatter same.isEditable

        input =
          Tuple.first newDataRes |>
          Result.map formatter |>
          Result.withDefault "<error setting value!>"
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
      SelectTextMsg ctrl selMsg -> -- field select list messages
        applySelect ctrl same (toMsg << SelectTextMsg ctrl) selMsg

      -- user input messages
      OnMsg ctrl value ->
        applyInput (toMsg << SelectTextMsg ctrl) ctrl value

      OnFocusMsg ctrl focus ->
        setEditing ctrl initTextSelect focus

      OnTextSelect ctrl value -> -- text selected from select component
        (applySelectedValue ctrl value <| JM.data model)

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
