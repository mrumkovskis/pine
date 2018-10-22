module Select exposing
  ( SelectModel, Msg, init, searchParamName, additionalParams
  , onSelectInput, onMouseSelect, search, update
  )

{-| Model and controller for typeahead (a.k.a autocomplete, suggestion) data.
-}

import JsonModel as JM
import SelectEvents as SE
import Utils

import Html exposing (Attribute)
import Html.Events exposing (onInput, onMouseDown)
import Task

import Debug exposing (log)


type alias SelectModel msg value =
  { model: JM.ListModel msg value
  , search: String
  , additionalParams: JM.SearchParams
  , toSelectedmsg: value -> msg
  , activeIdx: Maybe Int
  , searchParamName: String
  , active: Bool
  , toString: value -> String
  }


type Msg msg value
  = Navigate SE.Msg
  | Search String
  | SetActive Bool Int
  | Select
  | ModelMsg (JM.ListMsg msg value)


type alias Tomsg msg value = (Msg msg value) -> msg


init: JM.ListModel msg value -> String -> (value -> msg) -> (value -> String) -> SelectModel msg value
init model search toSelectedmsg toString =
  SelectModel model search [] toSelectedmsg Nothing "search" False toString


searchParamName: String -> SelectModel msg value -> SelectModel msg value
searchParamName name model =
  { model | searchParamName = name }


additionalParams: JM.SearchParams -> SelectModel msg value -> SelectModel msg value
additionalParams params model =
  { model | additionalParams = params }


onSelectInput: Tomsg msg value -> List (Attribute msg)
onSelectInput toMsg = [ SE.onNavigation (toMsg << Navigate) ]


onMouseSelect: Tomsg msg value -> Int -> List (Attribute msg)
onMouseSelect toMsg idx = [ onMouseDown (toMsg <| SetActive True idx) ]


search: Tomsg msg value -> String -> Cmd msg
search toMsg search = Task.perform (toMsg << Search) <| Task.succeed search


update: Tomsg msg value -> Msg msg value -> SelectModel msg value -> (SelectModel msg value, Cmd msg)
update toMsg msg ({model, activeIdx, toSelectedmsg, searchParamName, active} as same) =
  let
    maybeSelectCmd idx =
      Utils.at idx (JM.data model) |>
      Maybe.map (\value -> [ Task.perform toSelectedmsg <| Task.succeed value ]) |>
      Maybe.withDefault []

    findIdx model =
      JM.data model |>
      List.map same.toString |>
      Utils.matchIdx same.search
  in
    case msg of
      Navigate action ->
        let
          size = List.length (JM.data model)

          upDownModel init delta =
            let
              idx =
                activeIdx |>
                Maybe.withDefault init |>
                Just |>
                Maybe.map ((+) delta) |>
                Maybe.andThen
                  (\i ->
                    if i >= 0 && i < size then Just i else Nothing
                  )
            in
              ( if active then { same | activeIdx = idx }
                else { same | active = True }
              ) !
                (if not active && JM.isEmpty model then -- if list collapsed and empty try search
                    [ Task.perform (toMsg << Search) <| Task.succeed same.search ]
                  else
                    []
                )
        in
          case action of
            SE.Up ->
              upDownModel size -1

            SE.Down ->
              upDownModel -1 1

            SE.Esc ->
              { same | active = False }
                ! []

            SE.Select ->
              same
                ! (if active then [ Task.perform toMsg <| Task.succeed Select ] else [])

      Search search ->
        { same | active = True, search = search }
          ! [ JM.fetch (toMsg << ModelMsg) <| [(searchParamName, search)] ++ same.additionalParams ]

      SetActive select idx ->
        { same | activeIdx = Just idx }
          ! (if select then maybeSelectCmd idx else [])

      Select ->
        { same | active = False }
          ! (activeIdx |> Maybe.map maybeSelectCmd |> Maybe.withDefault [])

      ModelMsg data ->
        JM.update (toMsg << ModelMsg) data model |>
        Tuple.mapFirst (\m -> { same | model = m, activeIdx = findIdx m })
