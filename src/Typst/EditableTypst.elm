module Typst.EditableTypst exposing (Model, Msg, OutMsg(..), ViewConfig, currentSource, init, initWithSource, isBlank, requestFocus, update, view)

import Dict exposing (Dict)
import Html exposing (Html, node)
import Html.Attributes exposing (attribute, class, property)
import Html.Events exposing (on)
import Json.Decode as Decode
import Json.Encode as Encode
import Typst.Port as Port


type alias Model =
    { id : String
    , fieldPlaceholder : String
    , shortcutHint : String
    , committedSource : String
    , liveSource : String
    }


init : String -> String -> String -> Model
init id fieldPlaceholder shortcutHint =
    { id = id
    , fieldPlaceholder = fieldPlaceholder
    , shortcutHint = shortcutHint
    , committedSource = ""
    , liveSource = ""
    }


initWithSource : String -> String -> String -> String -> Model
initWithSource id fieldPlaceholder shortcutHint existingSource =
    { id = id
    , fieldPlaceholder = fieldPlaceholder
    , shortcutHint = shortcutHint
    , committedSource = existingSource
    , liveSource = existingSource
    }


isBlank : Model -> Bool
isBlank model =
    String.trim (currentSource model) == ""


currentSource : Model -> String
currentSource model =
    model.liveSource


type Msg
    = LiveInput String
    | Committed String
    | ImageReceived String String
    | FocusRequested


{-| Opaque: callers never need to know which Msg it maps to, only that
sending it starts editing and moves the OS keyboard focus into this field,
synchronously (via the `focusField` port) within the originating keydown's
own call stack.
-}
requestFocus : Msg
requestFocus =
    FocusRequested


type OutMsg
    = NoOutMsg
    | SourceCommitted String
    | ImageAdded String String


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        LiveInput source ->
            ( { model | liveSource = source }, Cmd.none, NoOutMsg )

        Committed source ->
            let
                changed =
                    source /= model.committedSource
            in
            ( { model | committedSource = source, liveSource = source }
            , Cmd.none
            , if changed then
                SourceCommitted source

              else
                NoOutMsg
            )

        ImageReceived imgId data ->
            ( model, Cmd.none, ImageAdded imgId data )

        FocusRequested ->
            ( model, Port.focusField model.id, NoOutMsg )


{-| Not part of Model: `preamble`/`knownImages` can change independently of
any particular field (e.g. a new image pasted into the other field), and
`theme` is purely a render-time concern now that recompiling on theme change
is just an attribute change picked up by <typst-note-editor> itself.
-}
type alias ViewConfig =
    { preamble : String
    , knownImages : Dict String String
    , theme : String
    , nextField : Maybe String
    , submitSelector : Maybe String
    }


view : ViewConfig -> Model -> Html Msg
view config model =
    node "typst-note-editor"
        (List.concat
            [ [ class "review-box"
              , attribute "field-id" model.id
              , attribute "source" model.committedSource
              , attribute "preamble" config.preamble
              , attribute "placeholder" model.fieldPlaceholder
              , attribute "shortcut-hint" model.shortcutHint
              , attribute "theme" config.theme
              , property "knownImages" (encodeImages config.knownImages)
              , on "tide-note-committed" (Decode.map Committed detailValueDecoder)
              , on "tide-note-input" (Decode.map LiveInput detailValueDecoder)
              , on "tide-image-added" imageEventDecoder
              ]
            , config.nextField |> Maybe.map (\f -> [ attribute "next-field" f ]) |> Maybe.withDefault []
            , config.submitSelector |> Maybe.map (\s -> [ attribute "submit-selector" s ]) |> Maybe.withDefault []
            ]
        )
        []


detailValueDecoder : Decode.Decoder String
detailValueDecoder =
    Decode.at [ "detail", "value" ] Decode.string


imageEventDecoder : Decode.Decoder Msg
imageEventDecoder =
    Decode.map2 ImageReceived
        (Decode.at [ "detail", "id" ] Decode.string)
        (Decode.at [ "detail", "data" ] Decode.string)


encodeImages : Dict String String -> Encode.Value
encodeImages images =
    Encode.object (Dict.toList images |> List.map (Tuple.mapSecond Encode.string))
