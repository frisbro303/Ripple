module Pages.Settings exposing (Model, Msg, SyncUpdate(..), applySyncedPreamble, applySyncedRetention, dailyNewLimit, decodeFromStore, default, deferDays, desiredRetention, request, subscriptions, typstPreamble, update, view)

import Browser.Events
import Html exposing (Html, div, h3, label, option, select, text, textarea)
import Html.Attributes as Attr exposing (attribute, class, for, id, placeholder, selected, spellcheck, style, type_, value)
import Html.Events exposing (on, onBlur, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import Local.Store as Store
import Theme exposing (Theme)
import Typst.Highlight as Highlight
import Typst.Keymap as Keymap
import Typst.Port as Port


type alias Model =
    { retentionPercent : Int
    , retentionInput : String
    , dailyNewLimit : Int
    , dailyNewLimitInput : String
    , deferDays : Int
    , deferDaysInput : String
    , typstPreamble : String
    , theme : Theme
    , highlightTree : Maybe Highlight.Node
    , fieldHeight : Float
    , drag : Maybe Drag
    , scrollTop : Float
    }


type alias Drag =
    { startY : Float
    , startHeight : Float
    }


defaultFieldHeight : Float
defaultFieldHeight =
    128


minFieldHeight : Float
minFieldHeight =
    64


maxFieldHeight : Float
maxFieldHeight =
    480


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
    , highlightTree = Nothing
    , fieldHeight = defaultFieldHeight
    , drag = Nothing
    , scrollTop = 0
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
                    ( model
                    , Cmd.batch
                        [ Port.highlightTypst preambleFieldId model.typstPreamble
                        , Theme.setTheme model.theme
                        ]
                    )
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
    | GotHighlightTree String Decode.Value
    | HandlePressed Float
    | HandleDragged Float
    | HandleReleased
    | Scrolled Float
    | KeyOutcome Keymap.Outcome


type SyncUpdate
    = NoSyncUpdate
    | PreambleCommitted String
    | RetentionCommitted Int


update : Msg -> Model -> ( Model, Cmd Msg, SyncUpdate )
update msg model =
    case msg of
        RetentionChanged raw ->
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
            ( { model | typstPreamble = text_ }, Port.highlightTypst preambleFieldId text_, NoSyncUpdate )

        PreambleBlurred ->
            ( model, save model, PreambleCommitted model.typstPreamble )

        KeyOutcome outcome ->
            case outcome of
                Keymap.PassThrough ->
                    ( model, Cmd.none, NoSyncUpdate )

                Keymap.Blur ->
                    ( model, Port.blurField preambleFieldId, NoSyncUpdate )

                Keymap.Chain ->
                    ( model, Cmd.none, NoSyncUpdate )

                Keymap.Replace { value, start, end } ->
                    ( { model | typstPreamble = value }
                    , Cmd.batch [ Port.highlightTypst preambleFieldId value, Port.setSelection preambleFieldId start end ]
                    , NoSyncUpdate
                    )

        GotHighlightTree requestId value ->
            if requestId /= preambleFieldId then
                ( model, Cmd.none, NoSyncUpdate )

            else
                ( { model | highlightTree = Decode.decodeValue Highlight.decoder value |> Result.toMaybe }
                , Cmd.none
                , NoSyncUpdate
                )

        HandlePressed clientY ->
            ( { model | drag = Just { startY = clientY, startHeight = model.fieldHeight } }, Cmd.none, NoSyncUpdate )

        HandleDragged clientY ->
            case model.drag of
                Just drag ->
                    ( { model | fieldHeight = clamp minFieldHeight maxFieldHeight (drag.startHeight + (clientY - drag.startY)) }
                    , Cmd.none
                    , NoSyncUpdate
                    )

                Nothing ->
                    ( model, Cmd.none, NoSyncUpdate )

        HandleReleased ->
            ( { model | drag = Nothing }, Cmd.none, NoSyncUpdate )

        Scrolled scrollTop ->
            ( { model | scrollTop = scrollTop }, Cmd.none, NoSyncUpdate )


applySyncedPreamble : String -> Model -> ( Model, Cmd Msg )
applySyncedPreamble preamble model =
    let
        newModel =
            { model | typstPreamble = preamble }
    in
    ( newModel, Cmd.batch [ save newModel, Port.highlightTypst preambleFieldId preamble ] )


applySyncedRetention : Int -> Model -> ( Model, Cmd Msg )
applySyncedRetention retentionPercent model =
    let
        newModel =
            { model | retentionPercent = retentionPercent, retentionInput = String.fromInt retentionPercent }
    in
    ( newModel, save newModel )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.typstHighlighted GotHighlightTree
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (Decode.map HandleDragged (Decode.field "clientY" Decode.float))
                    , Browser.Events.onMouseUp (Decode.succeed HandleReleased)
                    ]

            Nothing ->
                Sub.none
        ]


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
                    [ div
                        [ class "note-editor-field-wrap"
                        , style "height" (String.fromFloat model.fieldHeight ++ "px")
                        ]
                        [ div [ class "note-editor-highlight" ]
                            [ div
                                [ class "note-editor-highlight-scroll"
                                , style "transform" ("translateY(-" ++ String.fromFloat model.scrollTop ++ "px)")
                                ]
                                [ case model.highlightTree of
                                    Just tree ->
                                        Highlight.view tree

                                    Nothing ->
                                        text model.typstPreamble
                                ]
                            ]
                        , textarea
                            [ id preambleFieldId
                            , class "note-editor-field"
                            , placeholder "Typst preamble"
                            , value model.typstPreamble
                            , attribute "autocorrect" "off"
                            , attribute "autocapitalize" "off"
                            , spellcheck False
                            , onInput PreambleChanged
                            , onBlur PreambleBlurred
                            , on "scroll" (Decode.map Scrolled (Decode.at [ "target", "scrollTop" ] Decode.float))
                            , Keymap.onKeyDown False KeyOutcome
                            ]
                            []
                        , div
                            [ class "note-editor-resize-handle"
                            , preventDefaultOn "mousedown"
                                (Decode.map (\clientY -> ( HandlePressed clientY, True )) (Decode.field "clientY" Decode.float))
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


settingsSection : String -> List (Html Msg) -> Html Msg
settingsSection title fields =
    div [ class "settings-section" ]
        [ h3 [ class "history-heading" ] [ text title ]
        , div [ class "settings-fields" ] fields
        ]
