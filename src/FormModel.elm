module FormModel exposing
    ( Model
    , Msg
    , Tomsg
    , cancel
    , cancelMsg
    , create
    , createMsg
    , delete
    , deleteMsg
    , edit
    , editMsg
    , fetch
    , fetchMsg
    , init
    , map
    , save
    , saveMsg
    , saveWithParams
    , saveWithParamsMsg
    , set
    , setMsg
    , toModelMsg
    , update
    )

import Ask
import EditModel as EM
import JsonModel as JM
import Utils exposing (..)


type alias Model msg =
    { init : () -> EM.EditModel msg
    , form : Maybe (EM.EditModel msg)
    , toMessagemsg : Ask.Tomsg msg
    }


type Msg msg
    = CreateMsg (EM.JsonEditMsg msg)
    | EditMsg JM.JsonValue
    | CancelEditMsg (Maybe (Cmd msg)) Bool
    | SaveMsg (Maybe (JM.JsonValue -> Cmd msg)) (EM.JsonEditMsg msg)
    | DeleteMsg (Maybe (JM.JsonValue -> Cmd msg)) Int
    | SetMsg JM.JsonValue


type alias Tomsg msg =
    Msg msg -> msg


init : (() -> EM.EditModel msg) -> Ask.Tomsg msg -> Model msg
init initializer =
    Model initializer Nothing


toModelMsg : Tomsg msg -> (EM.JsonEditMsg msg -> msg)
toModelMsg toMsg =
    toMsg << CreateMsg


createMsg : Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> msg
createMsg toMsg searchParams =
    EM.createMsg (toModelMsg toMsg) searchParams


create : Tomsg msg -> JM.SearchParams -> (JM.JsonValue -> JM.JsonValue) -> Cmd msg
create toMsg searchParams =
    domsg << createMsg toMsg searchParams


saveMsg : Tomsg msg -> Maybe (JM.JsonValue -> Cmd msg) -> msg
saveMsg toMsg maybeSuccessCmd =
    EM.saveMsg (toMsg << SaveMsg maybeSuccessCmd) []


save : Tomsg msg -> Maybe (JM.JsonValue -> Cmd msg) -> Cmd msg
save toMsg =
    domsg << saveMsg toMsg


saveWithParamsMsg : Tomsg msg -> JM.SearchParams -> Maybe (JM.JsonValue -> Cmd msg) -> msg
saveWithParamsMsg toMsg params maybeSuccessCmd =
    EM.saveMsg (toMsg << SaveMsg maybeSuccessCmd) params


saveWithParams : Tomsg msg -> JM.SearchParams -> Maybe (JM.JsonValue -> Cmd msg) -> Cmd msg
saveWithParams toMsg params =
    domsg << saveWithParamsMsg toMsg params


fetchMsg : Tomsg msg -> Int -> msg
fetchMsg toMsg id =
    editMsg toMsg <| JM.jsonEdit "id" (JM.JsNumber <| toFloat id) JM.jsonEmptyObj


fetch : Tomsg msg -> Int -> Cmd msg
fetch toMsg =
    domsg << fetchMsg toMsg


editMsg : Tomsg msg -> JM.JsonValue -> msg
editMsg toMsg =
    toMsg << EditMsg


edit : Tomsg msg -> JM.JsonValue -> Cmd msg
edit toMsg =
    domsg << editMsg toMsg


cancelMsg : Tomsg msg -> Maybe (Cmd msg) -> Bool -> msg
cancelMsg toMsg maybeCmd =
    toMsg << CancelEditMsg maybeCmd


cancel : Tomsg msg -> Maybe (Cmd msg) -> Bool -> Cmd msg
cancel toMsg maybeCmd =
    domsg << cancelMsg toMsg maybeCmd


deleteMsg : Tomsg msg -> Maybe (JM.JsonValue -> Cmd msg) -> Int -> msg
deleteMsg toMsg maybeSuccessCmd =
    toMsg << DeleteMsg maybeSuccessCmd


delete : Tomsg msg -> Maybe (JM.JsonValue -> Cmd msg) -> Int -> Cmd msg
delete toMsg maybeSuccessCmd =
    domsg << deleteMsg toMsg maybeSuccessCmd


setMsg : Tomsg msg -> JM.JsonValue -> msg
setMsg toMsg =
    toMsg << SetMsg


set : Tomsg msg -> JM.JsonValue -> Cmd msg
set toMsg =
    domsg << setMsg toMsg


map : (JM.JsonValue -> JM.JsonValue) -> Model msg -> Model msg
map mapper model =
    model.form
        |> Maybe.map (\f -> { f | model = JM.map mapper f.model })
        |> Maybe.map (\f -> { model | form = Just f })
        |> Maybe.withDefault model


update : Tomsg msg -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update toMsg msg ({ form, toMessagemsg } as model) =
    case msg of
        CreateMsg data ->
            form
                |> Utils.orElse (Just <| model.init ())
                |> Maybe.map (EM.update (toMsg << CreateMsg) data)
                |> Maybe.map
                    (Tuple.mapFirst (\m -> { model | form = Just m }))
                |> Maybe.withDefault ( model, Cmd.none )

        EditMsg data ->
            ( model
            , JM.jsonInt "id" data
                |> Maybe.map (EM.fetch (toMsg << CreateMsg))
                |> Maybe.withDefault Cmd.none
            )

        CancelEditMsg maybeCmd ask ->
            if ask && (model.form |> Maybe.map .isDirty |> Maybe.withDefault False) then
                ( model
                , Ask.ask
                    toMessagemsg
                    "Vai atcelt datu labošanu?"
                    (cancel toMsg maybeCmd False)
                    Nothing
                )

            else
                ( { model | form = Nothing }
                , maybeCmd |> Maybe.withDefault Cmd.none
                )

        SaveMsg maybeSuccessCmd data ->
            form
                |> Maybe.map (EM.update (toMsg << SaveMsg maybeSuccessCmd) data)
                |> Maybe.map (Tuple.mapFirst (\m -> { model | form = Just m }))
                |> Maybe.withDefault ( model, Cmd.none )
                |> (\( newmod, cmd ) ->
                        if cmd == Cmd.none then
                            ( newmod
                            , newmod.form
                                |> Maybe.map (.model >> JM.data)
                                |> Maybe.andThen (\m -> maybeSuccessCmd |> Maybe.map (\successCmd -> successCmd m))
                                |> Maybe.withDefault Cmd.none
                            )

                        else
                            ( newmod, cmd )
                   )

        DeleteMsg maybeSuccessCmd id ->
            Tuple.pair
                model
            <|
                Ask.ask
                    toMessagemsg
                    "Vai dzēst ierakstu?"
                    (EM.delete (toMsg << SaveMsg maybeSuccessCmd) id)
                    Nothing

        SetMsg data ->
            ( model, EM.set (toMsg << CreateMsg) <| always data )
