module Select exposing
  ( SelectModel, Msg, Tomsg, init, additionalParams, updateSearch, isLoading
  , onMouseSelect
  , navigationMsg, setActiveMsg, selectMsg, searchMsg
  , search, update
  )

{-| Model and controller for typeahead (a.k.a autocomplete, suggestion) data.

@docs SelectModel, Msg, init, additionalParams, updateSearch,
      onSelectInput, onMouseSelect, search, update
-}

import JsonModel as JM
import SelectEvents as SE
import Utils exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (..)
import Json.Decode as JD


{-| Model for select component -}
type alias SelectModel msg value =
  { model: JM.ListModel msg value
  , searchParamName: String
  , search: String
  , additionalParams: JM.SearchParams
  , toSelectedmsg: value -> msg
  , activeIdx: Maybe Int
  , active: Bool
  , toString: value -> String
  , filter: String -> JM.ListModel msg value -> JM.ListModel msg value
  }


{-| Message for select model update -}
type Msg msg value
  = Navigate SE.Msg
  | Search String
  | SetActive Int
  | SetActiveAndSelect Int
  | Select
  | ModelMsg (JM.ListMsg msg value)


type alias Tomsg msg value = (Msg msg value) -> msg


{-| Initializes SelectModel with [`JsonModel`](JsonModel), initial search string,
selected value message constructor and value to string converter.
-}
init: JM.ListModel msg value -> String -> String -> (value -> msg) -> (value -> String) -> SelectModel msg value
init model paramName initSearch toSelectedmsg toString =
  SelectModel model paramName initSearch [] toSelectedmsg Nothing False toString (always identity)


{-| Adds additional search parameters.
-}
additionalParams: JM.SearchParams -> SelectModel msg value -> SelectModel msg value
additionalParams params model =
  { model | additionalParams = params }


filter:
  (String -> JM.ListModel msg value -> JM.ListModel msg value) ->
  SelectModel msg value -> SelectModel msg value
filter filterFun model =
  { model | filter = filterFun }


{-| Updates search condition but does not perform search command
-}
updateSearch: String -> SelectModel msg value -> SelectModel msg value
updateSearch text model =
  { model | search = text }


{-| Calls isProgress function on underlying model -}
isLoading: SelectModel msg value -> Bool
isLoading { model } =
  JM.isProgress model


{-| Mouse down listener on list item specified with idx parameter.
-}
onMouseSelect: (Int -> msg) -> (Int -> msg) -> Int -> List (Attribute msg)
onMouseSelect setActive select idx =
  [ preventDefaultOn "mousedown" <| JD.succeed (setActive idx, True) -- prevent from loosing input focus, but mark active item
  , onMouseUp <| select idx
  ]


navigationMsg: Tomsg msg value -> SE.Msg -> msg
navigationMsg toMsg = toMsg << Navigate


setActiveMsg: Tomsg msg value -> Int -> msg
setActiveMsg toMsg =
  toMsg << SetActive


selectMsg: Tomsg msg value -> Int -> msg
selectMsg toMsg =
  toMsg << SetActiveAndSelect


searchMsg: Tomsg msg value -> String -> msg
searchMsg toMsg =
  toMsg << Search


{-| Search command.
-}
search: Tomsg msg value -> String -> Cmd msg
search toMsg = domsg << searchMsg toMsg


{-| Model update -}
update: Tomsg msg value -> Msg msg value -> SelectModel msg value -> (SelectModel msg value, Cmd msg)
update toMsg msg ({ model, activeIdx, toSelectedmsg, active } as same) =
  let
    selectCmd idx =
      Utils.at idx (JM.data model) |>
      Maybe.map (\value -> do toSelectedmsg <| value) |>
      Maybe.withDefault Cmd.none

    findIdx newModel =
      JM.data newModel |>
      List.map same.toString |>
      Utils.matchIdx same.search

    maybeActivateModel (newModel, cmd) =
     ( if cmd == Cmd.none then
          { newModel |
            active = True
          , model = newModel.filter newModel.search newModel.model
          }
        else
          newModel
     , cmd
     )

  in
    case msg of
      Navigate action ->
        let
          size = List.length (JM.data model)

          upDownModel initIdx delta =
            let
              idx =
                activeIdx |>
                Maybe.withDefault initIdx |>
                Just |>
                Maybe.map ((+) delta) |>
                Maybe.andThen
                  (\i ->
                    if i >= 0 && i < size then Just i else Nothing
                  )
            in
              ( if active then { same | activeIdx = idx } else same
              , if not active then -- if list collapsed try search
                  do (toMsg << Search) <| same.search
                else Cmd.none
              )
        in
          case action of
            SE.Up ->
              upDownModel size -1

            SE.Down ->
              upDownModel -1 1

            SE.Esc ->
              ( { same | active = False }, Cmd.none )

            SE.Select ->
              ( same
              , if active then do toMsg <| Select else Cmd.none
              )

      Search text ->
        ( { same | search = text }
        , JM.fetchFromStart
            (toMsg << ModelMsg) <|
            [(same.searchParamName, text)] ++ same.additionalParams
        )

      SetActive idx ->
        ( { same | activeIdx = Just idx }
        , Cmd.none
        )

      SetActiveAndSelect idx ->
        ( same
        , activeIdx |>
          Utils.filter ((==) idx) |>
          Maybe.map (\_ -> do toMsg <| Select) |>
          Maybe.withDefault Cmd.none
        )

      Select ->
        ( { same | active = False }
        , activeIdx |> Maybe.map selectCmd |> Maybe.withDefault Cmd.none
        )

      ModelMsg data ->
        JM.update (toMsg << ModelMsg) data model |>
        Tuple.mapFirst (\m -> { same | model = m, activeIdx = findIdx m }) |>
        maybeActivateModel
