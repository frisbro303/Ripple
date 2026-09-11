module Typst.EditableTypst exposing (Model, Msg, OutMsg(..), currentSource, init, initWithSource, isBlank, recompile, requestFocus, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, img, span, text, textarea)
import Html.Attributes exposing (attribute, class, classList, id, placeholder, spellcheck, src, style, value)
import Html.Events exposing (on, onClick, onFocus, onInput, preventDefaultOn)
import Json.Decode as Decode
import Random
import Task
import Time
import Typst.Highlight as Highlight
import Typst.Keymap as Keymap
import Typst.Port as Port
import UUID
import Url


type alias Model =
    { id : String
    , fieldPlaceholder : String
    , shortcutHint : String
    , preamble : String
    , chainable : Bool
    , committedSource : String
    , committedResult : Result String String
    , draftSource : String
    , draftResult : Result String String
    , manualEditing : Bool
    , pendingRefocus : Bool
    , fieldHeight : Float
    , drag : Maybe Drag
    , scrollTop : Float
    , highlightTree : Maybe Highlight.Node
    , knownImages : Dict String String
    , pendingImages : Dict String String
    }


type alias Drag =
    { startY : Float
    , startHeight : Float
    }


defaultFieldHeight : Float
defaultFieldHeight =
    96


minFieldHeight : Float
minFieldHeight =
    64


maxFieldHeight : Float
maxFieldHeight =
    480


init : String -> String -> String -> Bool -> String -> Dict String String -> Model
init id fieldPlaceholder shortcutHint chainable preamble knownImages =
    { id = id
    , fieldPlaceholder = fieldPlaceholder
    , shortcutHint = shortcutHint
    , preamble = preamble
    , chainable = chainable
    , committedSource = ""
    , committedResult = Err ""
    , draftSource = ""
    , draftResult = Err ""
    , manualEditing = False
    , pendingRefocus = False
    , fieldHeight = defaultFieldHeight
    , drag = Nothing
    , scrollTop = 0
    , highlightTree = Nothing
    , knownImages = knownImages
    , pendingImages = Dict.empty
    }


initWithSource : String -> String -> String -> Bool -> String -> Dict String String -> String -> ( Model, Cmd Msg )
initWithSource id fieldPlaceholder shortcutHint chainable preamble knownImages existingSource =
    ( { id = id
      , fieldPlaceholder = fieldPlaceholder
      , shortcutHint = shortcutHint
      , preamble = preamble
      , chainable = chainable
      , committedSource = existingSource
      , committedResult = Err ""
      , draftSource = existingSource
      , draftResult = Err ""
      , manualEditing = False
      , pendingRefocus = False
      , fieldHeight = defaultFieldHeight
      , drag = Nothing
      , scrollTop = 0
      , highlightTree = Nothing
      , knownImages = knownImages
      , pendingImages = Dict.empty
      }
    , if String.trim existingSource == "" then
        Cmd.none

      else
        Cmd.batch
            [ Port.compileTypst id preamble existingSource (imageAttachments knownImages Dict.empty existingSource)
            , Port.highlightTypst id existingSource
            ]
    )


isEditing : Model -> Bool
isEditing model =
    model.manualEditing || isBlank model


isBlank : Model -> Bool
isBlank model =
    String.trim (currentSource model) == ""


currentSource : Model -> String
currentSource model =
    if model.manualEditing then
        model.draftSource

    else
        model.committedSource


referencedImageIds : String -> List String
referencedImageIds source =
    String.split "#image(\"" source
        |> List.drop 1
        |> List.filterMap
            (\chunk ->
                case String.split "\"" chunk of
                    first :: _ ->
                        if String.endsWith ".png" first then
                            Just (String.dropRight 4 first)

                        else
                            Nothing

                    [] ->
                        Nothing
            )


imageAttachments : Dict String String -> Dict String String -> String -> List ( String, String )
imageAttachments knownImages pendingImages source =
    let
        allImages =
            Dict.union pendingImages knownImages
    in
    referencedImageIds source
        |> List.filterMap (\imgId -> Dict.get imgId allImages |> Maybe.map (\data -> ( imgId ++ ".png", data )))


type Msg
    = EditStarted
    | FocusRequested
    | DraftChanged String
    | ImageDataReceived { data : String, start : Int, end : Int }
    | GotTimeForImageInsert { data : String, start : Int, end : Int } Time.Posix
    | GotDraftResult String (Result String String)
    | GotHighlightTree String Decode.Value
    | Committed
    | NativeBlurred Bool
    | WindowFocusChanged Bool
    | OutsideClicked
    | HandlePressed Float
    | HandleDragged Float
    | HandleReleased
    | Scrolled Float
    | Recompile
    | KeyOutcome Keymap.Outcome


maxImageBase64Length : Int
maxImageBase64Length =
    2 * 1024 * 1024


requestFocus : Msg
requestFocus =
    FocusRequested


recompile : Msg
recompile =
    Recompile


type OutMsg
    = NoOutMsg
    | SourceCommitted String
    | ImageAdded String String
    | ShiftEnterChain


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        EditStarted ->
            if isEditing model then
                ( model, Cmd.none, NoOutMsg )

            else
                ( { model
                    | manualEditing = True
                    , draftSource = model.committedSource
                    , draftResult = model.committedResult
                  }
                , Port.focusField (textareaId model)
                , NoOutMsg
                )

        FocusRequested ->
            ( if model.manualEditing then
                model

              else
                { model
                    | manualEditing = True
                    , draftSource = model.committedSource
                    , draftResult = model.committedResult
                }
            , Port.focusField (textareaId model)
            , NoOutMsg
            )

        DraftChanged newSource ->
            ( { model | draftSource = newSource }
            , Cmd.batch
                [ Port.compileTypst model.id model.preamble newSource (imageAttachments model.knownImages model.pendingImages newSource)
                , Port.highlightTypst model.id newSource
                ]
            , NoOutMsg
            )

        ImageDataReceived imageData ->
            if String.length imageData.data > maxImageBase64Length then
                ( model
                , Port.alert "That image is too large to add to a card (limit ~1.5MB). Try a smaller image or a screenshot of just the relevant part."
                , NoOutMsg
                )

            else
                ( model, Task.perform (GotTimeForImageInsert imageData) Time.now, NoOutMsg )

        GotTimeForImageInsert { data, start, end } now ->
            let
                imgId =
                    Random.step UUID.generator (Random.initialSeed (Time.posixToMillis now))
                        |> Tuple.first
                        |> UUID.toString

                reference =
                    "#image(\"" ++ imgId ++ ".png\", width: 100%)"

                newValue =
                    String.left start model.draftSource ++ reference ++ String.dropLeft end model.draftSource

                newPos =
                    start + String.length reference

                newModel =
                    { model | draftSource = newValue, pendingImages = Dict.insert imgId data model.pendingImages }
            in
            ( newModel
            , Cmd.batch
                [ Port.compileTypst newModel.id newModel.preamble newValue (imageAttachments newModel.knownImages newModel.pendingImages newValue)
                , Port.highlightTypst newModel.id newValue
                , Port.setSelection (textareaId newModel) newPos newPos
                ]
            , ImageAdded imgId data
            )

        GotDraftResult requestId result ->
            if requestId /= model.id then
                ( model, Cmd.none, NoOutMsg )

            else if isEditing model then
                ( { model | draftResult = result }, Cmd.none, NoOutMsg )

            else
                ( { model | committedResult = result }, Cmd.none, NoOutMsg )

        GotHighlightTree requestId value ->
            if requestId /= model.id then
                ( model, Cmd.none, NoOutMsg )

            else
                ( { model | highlightTree = Decode.decodeValue Highlight.decoder value |> Result.toMaybe }
                , Cmd.none
                , NoOutMsg
                )

        Committed ->
            let
                changed =
                    model.draftSource /= model.committedSource
            in
            ( { model
                | manualEditing = False
                , committedSource = model.draftSource
                , committedResult = model.draftResult
              }
            , Cmd.none
            , if changed then
                SourceCommitted model.draftSource

              else
                NoOutMsg
            )

        NativeBlurred windowHasFocus ->
            if windowHasFocus then
                update Committed model

            else
                ( { model | pendingRefocus = True }, Cmd.none, NoOutMsg )

        WindowFocusChanged hasFocus ->
            if hasFocus && model.pendingRefocus then
                ( { model | pendingRefocus = False }, Port.focusField (textareaId model), NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        OutsideClicked ->
            let
                ( committedModel, _, outMsg ) =
                    update Committed model
            in
            ( committedModel, Port.blurField (textareaId model), outMsg )

        HandlePressed clientY ->
            ( { model | drag = Just { startY = clientY, startHeight = model.fieldHeight } }
            , Cmd.none
            , NoOutMsg
            )

        HandleDragged clientY ->
            case model.drag of
                Just drag ->
                    ( { model | fieldHeight = clamp minFieldHeight maxFieldHeight (drag.startHeight + (clientY - drag.startY)) }
                    , Cmd.none
                    , NoOutMsg
                    )

                Nothing ->
                    ( model, Cmd.none, NoOutMsg )

        HandleReleased ->
            ( { model | drag = Nothing }, Cmd.none, NoOutMsg )

        Scrolled scrollTop ->
            ( { model | scrollTop = scrollTop }, Cmd.none, NoOutMsg )

        Recompile ->
            let
                source =
                    currentSource model
            in
            if String.trim source == "" then
                ( model, Cmd.none, NoOutMsg )

            else
                ( model
                , Port.compileTypst model.id model.preamble source (imageAttachments model.knownImages model.pendingImages source)
                , NoOutMsg
                )

        KeyOutcome outcome ->
            case outcome of
                Keymap.PassThrough ->
                    ( model, Cmd.none, NoOutMsg )

                Keymap.Blur ->
                    ( model, Port.blurField (textareaId model), NoOutMsg )

                Keymap.Chain ->
                    ( model, Cmd.none, ShiftEnterChain )

                Keymap.Replace { value, start, end } ->
                    ( { model | draftSource = value }
                    , Cmd.batch
                        [ Port.compileTypst model.id model.preamble value (imageAttachments model.knownImages model.pendingImages value)
                        , Port.highlightTypst model.id value
                        , Port.setSelection (textareaId model) start end
                        ]
                    , NoOutMsg
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.typstCompiled GotDraftResult
        , Port.typstHighlighted GotHighlightTree
        , Port.windowFocusChanged WindowFocusChanged
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (Decode.map HandleDragged (Decode.field "clientY" Decode.float))
                    , Browser.Events.onMouseUp (Decode.succeed HandleReleased)
                    ]

            Nothing ->
                Sub.none
        , if model.manualEditing then
            Browser.Events.onMouseDown (outsideClickDecoder (boxId model))

          else
            Sub.none
        ]


outsideClickDecoder : String -> Decode.Decoder Msg
outsideClickDecoder boxIdValue =
    Decode.field "target" (isOutsideOf boxIdValue)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed OutsideClicked

                else
                    Decode.fail "click was inside the box"
            )


isOutsideOf : String -> Decode.Decoder Bool
isOutsideOf boxIdValue =
    Decode.field "id" Decode.string
        |> Decode.andThen
            (\nodeId ->
                if nodeId == boxIdValue then
                    Decode.succeed False

                else
                    Decode.oneOf
                        [ Decode.field "parentNode" (Decode.lazy (\_ -> isOutsideOf boxIdValue))
                        , Decode.succeed True
                        ]
            )


boxId : Model -> String
boxId model =
    "review-box-" ++ model.id


imageDataDecoder : Decode.Decoder Msg
imageDataDecoder =
    Decode.map3 (\data start end -> ImageDataReceived { data = data, start = start, end = end })
        (Decode.at [ "detail", "data" ] Decode.string)
        (Decode.at [ "target", "selectionStart" ] Decode.int)
        (Decode.at [ "target", "selectionEnd" ] Decode.int)


nativeBlurDecoder : Decode.Decoder Msg
nativeBlurDecoder =
    Decode.map NativeBlurred (Decode.at [ "detail", "windowHasFocus" ] Decode.bool)


view : Model -> Html Msg
view model =
    div
        [ id (boxId model)
        , class "review-box"
        , classList [ ( "review-box--editing", isEditing model ) ]
        , onClick EditStarted
        ]
        [ div [ class "editable-typst-edit" ]
            (div
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
                                text model.draftSource
                        ]
                    ]
                , textarea
                    [ id (textareaId model)
                    , class "note-editor-field"
                    , placeholder model.fieldPlaceholder
                    , value model.draftSource
                    , attribute "autocorrect" "off"
                    , attribute "autocapitalize" "off"
                    , spellcheck False
                    , onInput DraftChanged
                    , on "typst-blur" nativeBlurDecoder
                    , onFocus FocusRequested
                    , on "scroll" (Decode.map Scrolled (Decode.at [ "target", "scrollTop" ] Decode.float))
                    , on "typst-image-data" imageDataDecoder
                    , Keymap.onKeyDown model.chainable KeyOutcome
                    ]
                    []
                , div
                    [ class "note-editor-resize-handle"
                    , preventDefaultOn "mousedown"
                        (Decode.map (\clientY -> ( HandlePressed clientY, True )) (Decode.field "clientY" Decode.float))
                    ]
                    []
                ]
                :: (if String.trim model.draftSource == "" then
                        []

                    else
                        [ div [ class "note-editor-preview" ] [ previewView model.draftResult ] ]
                   )
            )
        , div [ class "editable-typst-preview-layer" ]
            [ previewView model.committedResult
            , span [ class "editable-typst-hint" ] [ text model.shortcutHint ]
            ]
        ]


previewView : Result String String -> Html msg
previewView result =
    case result of
        Ok svg ->
            img [ src (svgDataUrl svg) ] []

        Err error ->
            div [ class "note-editor-error" ] [ text error ]


svgDataUrl : String -> String
svgDataUrl svg =
    "data:image/svg+xml;charset=utf-8," ++ Url.percentEncode svg


textareaId : Model -> String
textareaId model =
    "editable-typst-" ++ model.id
