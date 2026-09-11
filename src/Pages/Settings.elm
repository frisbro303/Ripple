module Pages.Settings exposing (Model, Msg, SyncUpdate(..), applySyncedPreamble, applySyncedRetention, dailyNewLimit, decodeFromStore, default, deferDays, desiredRetention, request, typstPreamble, update, view)

import Html exposing (Html, div, h3, label, node, option, select, text)
import Html.Attributes exposing (attribute, class, for, id, selected, type_, value)
import Html.Attributes as Attr
import Html.Events exposing (on, onBlur, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Local.Store as Store
import Theme exposing (Theme)


type alias Model =
    { retentionPercent : Int
    , retentionInput : String
    , dailyNewLimit : Int
    , dailyNewLimitInput : String
    , deferDays : Int
    , deferDaysInput : String
    , typstPreamble : String
    , theme : Theme
    }


default : Model
default =
    { retentionPercent = 90
    , retentionInput = "90"
    , dailyNewLimit = 20
    , dailyNewLimitInput = "20"
    , deferDays = 2
    , deferDaysInput = "2"
    , typstPreamble = ""
    , theme = Theme.System
    }


desiredRetention : Model -> Float
desiredRetention model =
    toFloat model.retentionPercent / 100


typstPreamble : Model -> String
typstPreamble model =
    model.typstPreamble


dailyNewLimit : Model -> Int
dailyNewLimit model =
    model.dailyNewLimit


deferDays : Model -> Int
deferDays model =
    model.deferDays


key : String
key =
    "settings"


encoder : Model -> Encode.Value
encoder model =
    Encode.object
        [ ( "retentionPercent", Encode.int model.retentionPercent )
        , ( "dailyNewLimit", Encode.int model.dailyNewLimit )
        , ( "deferDays", Encode.int model.deferDays )
        , ( "typstPreamble", Encode.string model.typstPreamble )
        , ( "theme", Encode.string (Theme.toString model.theme) )
        ]


type alias StoredFields =
    { retentionPercent : Maybe Int
    , dailyNewLimit : Maybe Int
    , deferDays : Maybe Int
    , typstPreamble : Maybe String
    , theme : Maybe String
    }


decoder : Decode.Decoder StoredFields
decoder =
    Decode.map5 StoredFields
        (Decode.maybe (Decode.field "retentionPercent" Decode.int))
        (Decode.maybe (Decode.field "dailyNewLimit" Decode.int))
        (Decode.maybe (Decode.field "deferDays" Decode.int))
        (Decode.maybe (Decode.field "typstPreamble" Decode.string))
        (Decode.maybe (Decode.field "theme" Decode.string))


decodeFromStore : String -> Decode.Value -> Maybe ( Model, Cmd Msg )
decodeFromStore loadedKey val =
    if loadedKey == key then
        Decode.decodeValue decoder val
            |> Result.toMaybe
            |> Maybe.map
                (\fields ->
                    let
                        retentionPercent =
                            Maybe.withDefault default.retentionPercent fields.retentionPercent

                        dailyNewLimitValue =
                            Maybe.withDefault default.dailyNewLimit fields.dailyNewLimit

                        deferDaysValue =
                            Maybe.withDefault default.deferDays fields.deferDays

                        model =
                            { default
                                | retentionPercent = retentionPercent
                                , retentionInput = String.fromInt retentionPercent
                                , dailyNewLimit = dailyNewLimitValue
                                , dailyNewLimitInput = String.fromInt dailyNewLimitValue
                                , deferDays = deferDaysValue
                                , deferDaysInput = String.fromInt deferDaysValue
                                , typstPreamble = Maybe.withDefault default.typstPreamble fields.typstPreamble
                                , theme = fields.theme |> Maybe.map Theme.fromString |> Maybe.withDefault default.theme
                            }
                    in
                    ( model, Theme.setTheme model.theme )
                )

    else
        Nothing


request : Cmd msg
request =
    Store.get key


save : Model -> Cmd msg
save model =
    Store.set key (encoder model)


preambleFieldId : String
preambleFieldId =
    "settings-preamble"


type Msg
    = RetentionChanged String
    | RetentionBlurred
    | DailyNewLimitChanged String
    | DailyNewLimitBlurred
    | DeferDaysChanged String
    | DeferDaysBlurred
    | ThemeChanged String
    | PreambleChanged String
    | PreambleBlurred


type SyncUpdate
    = NoSyncUpdate
    | PreambleCommitted String
    | RetentionCommitted Int


update : Msg -> Model -> ( Model, Cmd Msg, SyncUpdate )
update msg model =
    case msg of
        RetentionChanged raw ->
            -- Deliberately not parsed/clamped here — doing that on every
            -- keystroke fought the user mid-typing (e.g. typing "75" would
            -- clamp the leading "7" to 50 before the second digit landed),
            -- and rejecting unparseable input (like a momentarily empty
            -- field) left the displayed value stuck out of sync with the
            -- model, since an unchanged model produces no DOM patch. The
            -- raw text is only parsed and clamped on blur, once the user's
            -- done typing.
            ( { model | retentionInput = raw }, Cmd.none, NoSyncUpdate )

        RetentionBlurred ->
            let
                percent =
                    String.toInt model.retentionInput
                        |> Maybe.map (clamp 50 99)
                        |> Maybe.withDefault model.retentionPercent

                newModel =
                    { model | retentionPercent = percent, retentionInput = String.fromInt percent }
            in
            ( newModel, save newModel, RetentionCommitted percent )

        DailyNewLimitChanged raw ->
            ( { model | dailyNewLimitInput = raw }, Cmd.none, NoSyncUpdate )

        DailyNewLimitBlurred ->
            let
                n =
                    String.toInt model.dailyNewLimitInput
                        |> Maybe.map (clamp 0 500)
                        |> Maybe.withDefault model.dailyNewLimit

                newModel =
                    { model | dailyNewLimit = n, dailyNewLimitInput = String.fromInt n }
            in
            ( newModel, save newModel, NoSyncUpdate )

        DeferDaysChanged raw ->
            ( { model | deferDaysInput = raw }, Cmd.none, NoSyncUpdate )

        DeferDaysBlurred ->
            let
                n =
                    String.toInt model.deferDaysInput
                        |> Maybe.map (clamp 1 60)
                        |> Maybe.withDefault model.deferDays

                newModel =
                    { model | deferDays = n, deferDaysInput = String.fromInt n }
            in
            ( newModel, save newModel, NoSyncUpdate )

        ThemeChanged raw ->
            let
                newModel =
                    { model | theme = Theme.fromString raw }
            in
            ( newModel, Cmd.batch [ save newModel, Theme.setTheme newModel.theme ], NoSyncUpdate )

        PreambleChanged text_ ->
            ( { model | typstPreamble = text_ }, Cmd.none, NoSyncUpdate )

        PreambleBlurred ->
            ( model, save model, PreambleCommitted model.typstPreamble )


applySyncedPreamble : String -> Model -> ( Model, Cmd Msg )
applySyncedPreamble preamble model =
    let
        newModel =
            { model | typstPreamble = preamble }
    in
    ( newModel, save newModel )


applySyncedRetention : Int -> Model -> ( Model, Cmd Msg )
applySyncedRetention retentionPercent model =
    let
        newModel =
            { model | retentionPercent = retentionPercent, retentionInput = String.fromInt retentionPercent }
    in
    ( newModel, save newModel )


view : Model -> Html Msg
view model =
    div [ class "settings-sections" ]
        [ settingsSection "Review"
            [ div [ class "settings-field" ]
                [ label [ for "settings-retention" ] [ text "Desired retention (%)" ]
                , Html.input
                    [ id "settings-retention"
                    , class "auth-input"
                    , type_ "number"
                    , Attr.min "50"
                    , Attr.max "99"
                    , value model.retentionInput
                    , onInput RetentionChanged
                    , onBlur RetentionBlurred
                    ]
                    []
                ]
            , div [ class "settings-field" ]
                [ label [ for "settings-daily-new-limit" ] [ text "New cards per day" ]
                , Html.input
                    [ id "settings-daily-new-limit"
                    , class "auth-input"
                    , type_ "number"
                    , Attr.min "0"
                    , Attr.max "500"
                    , value model.dailyNewLimitInput
                    , onInput DailyNewLimitChanged
                    , onBlur DailyNewLimitBlurred
                    ]
                    []
                ]
            , div [ class "settings-field" ]
                [ label [ for "settings-defer-days" ] [ text "Defer by (days)" ]
                , Html.input
                    [ id "settings-defer-days"
                    , class "auth-input"
                    , type_ "number"
                    , Attr.min "1"
                    , Attr.max "60"
                    , value model.deferDaysInput
                    , onInput DeferDaysChanged
                    , onBlur DeferDaysBlurred
                    ]
                    []
                ]
            ]
        , settingsSection "Appearance"
            [ div [ class "settings-field" ]
                [ label [ for "settings-theme" ] [ text "Theme" ]
                , select
                    [ id "settings-theme"
                    , class "auth-input"
                    , onInput ThemeChanged
                    ]
                    [ option [ value "system", selected (model.theme == Theme.System) ] [ text "System" ]
                    , option [ value "light", selected (model.theme == Theme.Light) ] [ text "Light" ]
                    , option [ value "dark", selected (model.theme == Theme.Dark) ] [ text "Dark" ]
                    ]
                ]
            ]
        , settingsSection "Typst"
            [ div [ class "settings-field" ]
                [ label [ for preambleFieldId ] [ text "Preamble" ]
                , div [ class "settings-preamble-wrap" ]
                    [ node "typst-code-field"
                        [ attribute "field-id" preambleFieldId
                        , attribute "placeholder" "Typst preamble"
                        , attribute "source" model.typstPreamble
                        , on "typst-input" (Decode.map PreambleChanged detailValueDecoder)
                        , on "typst-blur" (Decode.succeed PreambleBlurred)
                        ]
                        []
                    ]
                ]
            ]
        ]


detailValueDecoder : Decode.Decoder String
detailValueDecoder =
    Decode.at [ "detail", "value" ] Decode.string


settingsSection : String -> List (Html Msg) -> Html Msg
settingsSection title fields =
    div [ class "settings-section" ]
        [ h3 [ class "history-heading" ] [ text title ]
        , div [ class "settings-fields" ] fields
        ]
