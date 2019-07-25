module Select exposing
  ( SelectModel, Msg, Tomsg, init, additionalParams, updateSearch
  , onSelectInput, onMouseSelect, search, update
  )

{-| Model and controller for typeahead (a.k.a autocomplete, suggestion) data.

@docs SelectModel, Msg, init, additionalParams, updateSearch,
      onSelectInput, onMouseSelect, search, update
-}

import JsonModel as JM
import SelectEvents as SE
import Utils exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (onInput, onMouseDown)
import Task

import Debug exposing (log)


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
  }


{-| Message for select model update -}
type Msg msg value
  = Navigate SE.Msg
  | Search String
  | SetActiveAndSelect Int
  | Select
  | ModelMsg (JM.ListMsg msg value)


type alias Tomsg msg value = (Msg msg value) -> msg


{-| Initializes SelectModel with [`JsonModel`](JsonModel), initial search string,
selected value message constructor and value to string converter.
-}
init: JM.ListModel msg value -> String -> String -> (value -> msg) -> (value -> String) -> SelectModel msg value
init model paramName initSearch toSelectedmsg toString =
  SelectModel model paramName initSearch [] toSelectedmsg Nothing False toString


{-| Adds additional search parameters.
-}
additionalParams: JM.SearchParams -> SelectModel msg value -> SelectModel msg value
additionalParams params model =
  { model | additionalParams = params }


{-| Updates search condition but does not perform search command
-}
updateSearch: String -> SelectModel msg value -> SelectModel msg value
updateSearch text model =
  { model | search = text }


{-| Key listeners. Listens to key up, down, esc, enter.
-}
onSelectInput: Tomsg msg value -> List (Attribute msg)
onSelectInput toMsg = [ SE.onNavigation (toMsg << Navigate) ]


{-| Mouse down listener on list item specified with idx parameter.
-}
onMouseSelect: Tomsg msg value -> Int -> List (Attribute msg)
onMouseSelect toMsg idx = [ onMouseDown (toMsg <| SetActiveAndSelect idx) ]


{-| Search command.
-}
search: Tomsg msg value -> String -> Cmd msg
search toMsg text = do (toMsg << Search) <| text


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
     ( if cmd == Cmd.none then { newModel | active = True } else newModel, cmd )

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

      SetActiveAndSelect idx ->
        ( { same | activeIdx = Just idx }
        , do toMsg <| Select
        )

      Select ->
        ( { same | active = False }
        , activeIdx |> Maybe.map selectCmd |> Maybe.withDefault Cmd.none
        )

      ModelMsg data ->
        JM.update (toMsg << ModelMsg) data model |>
        Tuple.mapFirst (\m -> { same | model = m, activeIdx = findIdx m }) |>
        maybeActivateModel
