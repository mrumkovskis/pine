module ListModel exposing
    ( Model(..)
    , Msg
    , Tomsg
    , delayedCmd
    , init
    , load
    , loadAndReplaceUrl
    , loadAndReplaceUrlMsg
    , loadMore
    , loadMoreMsg
    , loadMsg
    , map
    , select
    , selectMsg
    , sort
    , sortMsg
    , subs
    , sync
    , syncMsg
    , toListMsg
    , toParamsMsg
    , update
    )

import Ask
import Dict exposing (fromList)
import EditModel as EM
import Html exposing (..)
import Html.Attributes exposing (..)
import JsonModel as JM
import Process
import ScrollEvents as SE
import Task
import Utils exposing (..)


type Model msg
    = Model
        { searchParams : EM.EditModel msg
        , list : JM.JsonListModel msg
        , stickyPos : Maybe SE.StickyElPos
        , sortCol : Maybe ( String, Bool )
        }


type Msg msg
    = ParamsMsg (EM.JsonEditMsg msg)
    | ListMsg (JM.JsonListMsg msg)
    | LoadMsg
    | LoadAndReplaceUrlMsg
    | LoadMoreMsg -- load more element visibility subscription message
    | ScrollEventsMsg (SE.Msg msg)
    | StickyPosMsg (Maybe SE.StickyElPos)
    | SortMsg String
    | SelectMsg (Bool -> JM.JsonValue -> msg) Bool JM.JsonValue
    | SyncMsg JM.SearchParams
    | DelayedCmdMsg (EM.EditModel msg -> Cmd msg)


type alias Tomsg msg =
    Msg msg -> msg


init : Tomsg msg -> EM.EditModel msg -> JM.JsonListModel msg -> Maybe JM.SearchParams -> ( Model msg, Cmd msg )
init toMsg searchPars l initPars =
    ( Model
        { searchParams = searchPars
        , list = l
        , stickyPos = Nothing
        , sortCol = initSortCol initPars
        }
    , initPars
        |> Maybe.map (sync toMsg)
        |> Maybe.withDefault (EM.set (toMsg << ParamsMsg) identity)
      -- initialize query form metadata
    )


toParamsMsg : Tomsg msg -> (EM.JsonEditMsg msg -> msg)
toParamsMsg toMsg =
    toMsg << ParamsMsg


toListMsg : Tomsg msg -> (JM.JsonListMsg msg -> msg)
toListMsg toMsg =
    toMsg << ListMsg


loadMsg : Tomsg msg -> msg
loadMsg toMsg =
    toMsg LoadMsg


load : Tomsg msg -> Cmd msg
load toMsg =
    domsg <| loadMsg toMsg


loadAndReplaceUrlMsg : Tomsg msg -> msg
loadAndReplaceUrlMsg toMsg =
    toMsg LoadAndReplaceUrlMsg


loadAndReplaceUrl : Tomsg msg -> Cmd msg
loadAndReplaceUrl toMsg =
    domsg <| loadAndReplaceUrlMsg toMsg


loadMoreMsg : Tomsg msg -> msg
loadMoreMsg toMsg =
    toMsg LoadMoreMsg


loadMore : Tomsg msg -> Cmd msg
loadMore toMsg =
    domsg <| loadMoreMsg toMsg


sortMsg : Tomsg msg -> String -> msg
sortMsg toMsg col =
    toMsg <| SortMsg col


sort : Tomsg msg -> String -> Cmd msg
sort toMsg col =
    domsg <| sortMsg toMsg col


selectMsg : Tomsg msg -> (Bool -> JM.JsonValue -> msg) -> Bool -> JM.JsonValue -> msg
selectMsg toMsg selectAction multiSelect val =
    toMsg <| SelectMsg selectAction multiSelect val


select : Tomsg msg -> (Bool -> JM.JsonValue -> msg) -> Bool -> JM.JsonValue -> Cmd msg
select toMsg selectAction multiSelect val =
    domsg <| selectMsg toMsg selectAction multiSelect val


syncMsg : Tomsg msg -> JM.SearchParams -> msg
syncMsg toMsg params =
    toMsg <| SyncMsg params


sync : Tomsg msg -> JM.SearchParams -> Cmd msg
sync toMsg params =
    domsg <| syncMsg toMsg params


delayedCmd : Tomsg msg -> Float -> (EM.EditModel msg -> Cmd msg) -> Cmd msg
delayedCmd toMsg delay cmdFunc =
    Task.perform
        toMsg
        (Process.sleep delay |> Task.map (always <| DelayedCmdMsg cmdFunc))


map : (JM.JsonValue -> JM.JsonValue) -> Model msg -> Model msg
map mapper (Model model) =
    Model { model | list = JM.mapList mapper model.list }


update : Tomsg msg -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update toMsg msg ((Model ({ searchParams, list, sortCol } as model)) as same) =
    let
        searchPars params =
            (JM.searchParsFromJson params |> List.filter (\( n, _ ) -> n /= "sort"))
                ++ (sortCol
                        |> Maybe.map
                            (\( c, o ) ->
                                [ ( "sort"
                                  , (if o then
                                        ""

                                     else
                                        "~"
                                    )
                                        ++ c
                                  )
                                ]
                            )
                        |> Maybe.withDefault []
                   )
    in
    case msg of
        ParamsMsg data ->
            EM.update (toMsg << ParamsMsg) data searchParams
                |> Tuple.mapFirst (\s -> Model { model | searchParams = s })

        ListMsg data ->
            JM.update (toMsg << ListMsg) data model.list
                |> Tuple.mapFirst (\m -> Model { model | list = m })

        SortMsg col ->
            Model
                { model
                    | sortCol =
                        if sortCol == Nothing then
                            Just ( col, True )

                        else
                            model.sortCol
                                |> Maybe.andThen
                                    (\( c, o ) ->
                                        if c == col then
                                            if not o then
                                                Nothing

                                            else
                                                Just ( c, False )

                                        else
                                            Just ( col, True )
                                    )
                }
                |> (\m -> ( m, load toMsg ))

        LoadMsg ->
            ( same
            , list
                |> (\(JM.Model d c) ->
                        searchPars searchParams.model
                            |> (\sp ->
                                    Utils.httpQuery sp
                                        |> (\pars ->
                                                if String.isEmpty pars then
                                                    "?"

                                                else
                                                    pars
                                           )
                                        |> (if sp == d.searchParams then
                                                Ask.replaceUrl c.toMessagemsg

                                            else
                                                Ask.pushUrl c.toMessagemsg
                                           )
                               )
                   )
            )

        LoadAndReplaceUrlMsg ->
            ( same
            , list
                |> (\(JM.Model _ c) ->
                        searchPars searchParams.model
                            |> (\sp ->
                                    Utils.httpQuery sp
                                        |> (\pars ->
                                                if String.isEmpty pars then
                                                    "?"

                                                else
                                                    pars
                                           )
                                        |> Ask.replaceUrl c.toMessagemsg
                               )
                   )
            )

        LoadMoreMsg ->
            ( same
            , JM.fetch (toMsg << ListMsg) (searchPars searchParams.model)
            )

        SelectMsg selmsg multiSelect data ->
            let
                set isSel d =
                    JM.jsonEdit "is_selected" (JM.JsBool isSel) d
            in
            JM.jsonBool "is_selected" data
                |> Maybe.withDefault False
                |> (\is_selected ->
                        ( not is_selected, set (not is_selected) data )
                   )
                |> (\( is_selected, newdata ) ->
                        ( is_selected
                        , newdata
                        , JM.data model.list
                            |> List.map
                                (\rec ->
                                    if rec == data then
                                        newdata

                                    else if not multiSelect && is_selected then
                                        set False rec

                                    else
                                        rec
                                )
                        )
                   )
                |> (\( is_selected, newdata, newlist ) ->
                        ( Model { model | list = JM.setData newlist model.list }
                        , do (selmsg is_selected) <| newdata
                        )
                   )

        SyncMsg params ->
            ( Model { model | sortCol = initSortCol (Just params) }
            , Cmd.batch
                [ JM.fetchFromStart (toMsg << ListMsg) params
                , params
                    |> List.map (Tuple.mapSecond JM.JsString)
                    |> (\vals ->
                            EM.set (toMsg << ParamsMsg) (\_ -> JM.JsObject <| Dict.fromList vals)
                       )

                -- set search form values
                ]
            )

        DelayedCmdMsg cmdFunc ->
            ( Model model, cmdFunc searchParams )

        ScrollEventsMsg data ->
            ( Model model, SE.process (toMsg << ScrollEventsMsg) data )

        StickyPosMsg pos ->
            ( Model { model | stickyPos = pos }, Cmd.none )


subs : Tomsg msg -> Maybe String -> Maybe String -> Maybe String -> Sub msg
subs toMsg loadMoreElId stickToElId stickId =
    let
        toSEMsg =
            toMsg << ScrollEventsMsg
    in
    Sub.batch
        [ loadMoreElId
            |> Maybe.map (\id -> SE.visibilitySub toSEMsg id <| toMsg LoadMoreMsg)
            |> Maybe.withDefault Sub.none
        , Maybe.map2
            (\stId id -> SE.stickySub toSEMsg stId id (toMsg << StickyPosMsg))
            stickToElId
            stickId
            |> Maybe.withDefault Sub.none
        ]


initSortCol : Maybe JM.SearchParams -> Maybe ( String, Bool )
initSortCol params =
    params
        |> Maybe.andThen (Utils.find (\( n, _ ) -> n == "sort"))
        |> Maybe.map Tuple.second
        |> Maybe.map
            (\c ->
                if String.startsWith "~" c then
                    ( String.dropLeft 1 c, False )

                else
                    ( c, True )
            )
