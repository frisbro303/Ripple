module Pages.Add exposing (Model, Msg, OutMsg(..), editBack, editFront, init, subscriptions, themeChanged, update, view)

import Dict exposing (Dict)
import Html exposing (Html, button, div, hr, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Ops.Op as Op exposing (Op, OpId(..), OpKind(..))
import Random
import Task
import Time
import Typst.Editor as Editor
import UUID


type alias Model =
    { front : Editor.Model
    , back : Editor.Model
    , preamble : String
    , knownImages : Dict String String
    , error : Maybe String
    }


init : String -> Dict String String -> Model
init preamble knownImages =
    { front = Editor.init "add-front" "Front" "i" True preamble knownImages
    , back = Editor.init "add-back" "Back" "o" True preamble knownImages
    , preamble = preamble
    , knownImages = knownImages
    , error = Nothing
    }


type Msg
    = FrontMsg Editor.Msg
    | BackMsg Editor.Msg
    | SubmitClicked
    | GotTimeForSubmit Time.Posix
    | CancelClicked
    | GotTimeForImageOp String String Time.Posix
    | ThemeChanged


type OutMsg
    = NoOutMsg
    | Submitted Op
    | Canceled
    | ImagePersisted Op


editFront : Msg
editFront =
    FrontMsg Editor.requestFocus


editBack : Msg
editBack =
    BackMsg Editor.requestFocus


themeChanged : Msg
themeChanged =
    ThemeChanged


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        FrontMsg frontMsg ->
            let
                ( frontModel, cmd, outMsg ) =
                    Editor.update frontMsg model.front

                modelAfterFront =
                    { model | front = frontModel }
            in
            case outMsg of
                Editor.ImageAdded imgId data ->
                    ( modelAfterFront, Cmd.batch [ Cmd.map FrontMsg cmd, Task.perform (GotTimeForImageOp imgId data) Time.now ], NoOutMsg )

                Editor.ShiftEnterChain ->
                    let
                        ( modelAfterChain, chainCmd, _ ) =
                            update editBack modelAfterFront
                    in
                    ( modelAfterChain, Cmd.batch [ Cmd.map FrontMsg cmd, chainCmd ], NoOutMsg )

                _ ->
                    ( modelAfterFront, Cmd.map FrontMsg cmd, NoOutMsg )

        BackMsg backMsg ->
            let
                ( backModel, cmd, outMsg ) =
                    Editor.update backMsg model.back

                modelAfterBack =
                    { model | back = backModel }
            in
            case outMsg of
                Editor.ImageAdded imgId data ->
                    ( modelAfterBack, Cmd.batch [ Cmd.map BackMsg cmd, Task.perform (GotTimeForImageOp imgId data) Time.now ], NoOutMsg )

                Editor.ShiftEnterChain ->
                    let
                        ( modelAfterSubmit, submitCmd, submitOut ) =
                            update SubmitClicked modelAfterBack
                    in
                    ( modelAfterSubmit, Cmd.batch [ Cmd.map BackMsg cmd, submitCmd ], submitOut )

                _ ->
                    ( modelAfterBack, Cmd.map BackMsg cmd, NoOutMsg )

        SubmitClicked ->
            if Editor.isBlank model.front || Editor.isBlank model.back then
                ( { model | error = Just "Both sides are required" }, Cmd.none, NoOutMsg )

            else
                ( { model | error = Nothing }, Task.perform GotTimeForSubmit Time.now, NoOutMsg )

        GotTimeForSubmit now ->
            let
                ( cardUuid, seed1 ) =
                    Random.step UUID.generator (Random.initialSeed (Time.posixToMillis now))

                ( opUuid, _ ) =
                    Random.step UUID.generator seed1

                op =
                    { id = OpId opUuid
                    , timeStamp = now
                    , opKind =
                        CreateCard
                            { id = cardUuid
                            , front = Editor.currentSource model.front
                            , back = Editor.currentSource model.back
                            }
                    }
            in
            ( init model.preamble model.knownImages, Cmd.none, Submitted op )

        CancelClicked ->
            ( init model.preamble model.knownImages, Cmd.none, Canceled )

        GotTimeForImageOp imgId data now ->
            let
                op =
                    { id = Op.newId now
                    , timeStamp = now
                    , opKind = AddImage { id = imgId, data = data }
                    }
            in
            ( model, Cmd.none, ImagePersisted op )

        ThemeChanged ->
            let
                ( frontModel, frontCmd, _ ) =
                    Editor.update Editor.recompile model.front

                ( backModel, backCmd, _ ) =
                    Editor.update Editor.recompile model.back
            in
            ( { model | front = frontModel, back = backModel }
            , Cmd.batch [ Cmd.map FrontMsg frontCmd, Cmd.map BackMsg backCmd ]
            , NoOutMsg
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map FrontMsg (Editor.subscriptions model.front)
        , Sub.map BackMsg (Editor.subscriptions model.back)
        ]


view : Model -> Html Msg
view model =
    div [ class "review-card-area" ]
        [ Html.map FrontMsg (Editor.view model.front)
        , hr [ class "review-divider" ] []
        , Html.map BackMsg (Editor.view model.back)
        , case model.error of
            Just err ->
                p [ class "note-editor-error" ] [ text err ]

            Nothing ->
                text ""
        , div [ class "review-actions" ]
            [ button [ id "add-submit-button", class "button-primary", onClick SubmitClicked ] [ text "Add" ]
            , button [ class "button-ghost", onClick CancelClicked ] [ text "Cancel" ]
            ]
        ]
