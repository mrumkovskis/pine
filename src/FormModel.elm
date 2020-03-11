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
    , set
    , setMsg
    , silentSave
    , silentSaveMsg
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
    , saveSuccessCmd : Maybe (JM.JsonValue -> Cmd msg)
    , toMessagemsg : Ask.Tomsg msg
    }


type Msg msg
    = CreateMsg (EM.JsonEditMsg msg)
    | EditMsg JM.JsonValue
    | CancelEditMsg (Maybe msg) Bool
    | SaveMsg Bool (Maybe (JM.JsonValue -> msg)) (EM.JsonEditMsg msg)
    | DeleteMsg (Maybe (JM.JsonValue -> msg)) Int
    | SetMsg JM.JsonValue


type alias Tomsg msg =
    Msg msg -> msg


init : (() -> EM.EditModel msg) -> Maybe (JM.JsonValue -> Cmd msg) -> Ask.Tomsg msg -> Model msg
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


saveMsg : Tomsg msg -> Maybe (JM.JsonValue -> msg) -> msg
saveMsg toMsg maybeSuccessmsg =
    EM.saveMsg (toMsg << SaveMsg False maybeSuccessmsg)


save : Tomsg msg -> Maybe (JM.JsonValue -> msg) -> Cmd msg
save toMsg =
    domsg << saveMsg toMsg


silentSaveMsg : Tomsg msg -> Maybe (JM.JsonValue -> msg) -> msg
silentSaveMsg toMsg maybeSuccessmsg =
    EM.saveMsg (toMsg << SaveMsg True maybeSuccessmsg)


silentSave : Tomsg msg -> Maybe (JM.JsonValue -> msg) -> Cmd msg
silentSave toMsg =
    domsg << silentSaveMsg toMsg


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


cancelMsg : Tomsg msg -> Maybe msg -> Bool -> msg
cancelMsg toMsg maybemsg =
    toMsg << CancelEditMsg maybemsg


cancel : Tomsg msg -> Maybe msg -> Bool -> Cmd msg
cancel toMsg maybemsg =
    domsg << cancelMsg toMsg maybemsg


deleteMsg : Tomsg msg -> Maybe (JM.JsonValue -> msg) -> Int -> msg
deleteMsg toMsg maybeSuccessmsg =
    toMsg << DeleteMsg maybeSuccessmsg


delete : Tomsg msg -> Maybe (JM.JsonValue -> msg) -> Int -> Cmd msg
delete toMsg maybeSuccessmsg =
    domsg << deleteMsg toMsg maybeSuccessmsg


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
update toMsg msg ({ form, saveSuccessCmd, toMessagemsg } as model) =
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

        CancelEditMsg maybeMsg ask ->
            if ask && (model.form |> Maybe.map .isDirty |> Maybe.withDefault False) then
                ( model
                , Ask.ask
                    toMessagemsg
                    "Vai atcelt datu labošanu?"
                    (cancel toMsg maybeMsg False)
                    Nothing
                )

            else
                ( { model | form = Nothing }
                , maybeMsg |> Maybe.map domsg |> Maybe.withDefault Cmd.none
                )

        SaveMsg isSilentSave maybeSuccessmsg data ->
            form
                |> Maybe.map (EM.update (toMsg << SaveMsg isSilentSave maybeSuccessmsg) data)
                |> Maybe.map (Tuple.mapFirst (\m -> { model | form = Just m }))
                |> Maybe.withDefault ( model, Cmd.none )
                |> (\( newmod, cmd ) ->
                        if cmd == Cmd.none then
                            ( newmod
                            , newmod.form
                                |> Maybe.map (.model >> JM.data)
                                |> Maybe.map
                                    (\m ->
                                        let
                                            saveCmd =
                                                saveSuccessCmd |> Utils.filter (always <| not isSilentSave)
                                        in
                                        case ( maybeSuccessmsg, saveCmd ) of
                                            ( Just sm, Just sc ) ->
                                                Cmd.batch [ domsg <| sm m, sc m ]

                                            ( Just sm, Nothing ) ->
                                                domsg <| sm m

                                            ( Nothing, Just sc ) ->
                                                sc m

                                            _ ->
                                                Cmd.none
                                    )
                                |> Maybe.withDefault Cmd.none
                            )

                        else
                            ( newmod, cmd )
                   )

        DeleteMsg maybeSuccessmsg id ->
            Tuple.pair
                model
            <|
                Ask.ask
                    toMessagemsg
                    "Vai dzēst ierakstu?"
                    (EM.delete (toMsg << SaveMsg True maybeSuccessmsg) id)
                    Nothing

        SetMsg data ->
            ( model, EM.set (toMsg << CreateMsg) <| always data )
