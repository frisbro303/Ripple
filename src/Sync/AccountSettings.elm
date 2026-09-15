module Sync.AccountSettings exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, form, h3, input, p, span, text)
import Html.Attributes exposing (class, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as Encode
import Sync.Auth exposing (authRequest, errorMessage, handleUnitResponse)
import Sync.Session exposing (Session, SessionUpdate(..))


type alias Model =
    { newPassword : String
    , newPasswordConfirm : String
    , passwordError : Maybe String
    , passwordSaved : Bool
    , newEmail : String
    , emailError : Maybe String
    , emailSaved : Bool
    }


init : Model
init =
    { newPassword = ""
    , newPasswordConfirm = ""
    , passwordError = Nothing
    , passwordSaved = False
    , newEmail = ""
    , emailError = Nothing
    , emailSaved = False
    }


type Msg
    = NewPasswordChanged String
    | NewPasswordConfirmChanged String
    | ChangePasswordClicked String
    | GotUpdatePasswordResponse (Result String ())
    | NewEmailChanged String
    | ChangeEmailClicked String
    | GotUpdateEmailResponse (Result String ())
    | LogoutClicked


update : Msg -> Model -> ( Model, Cmd Msg, SessionUpdate )
update msg model =
    case msg of
        NewPasswordChanged newPassword ->
            ( { model | newPassword = newPassword, passwordSaved = False }, Cmd.none, NoSessionChange )

        NewPasswordConfirmChanged newPasswordConfirm ->
            ( { model | newPasswordConfirm = newPasswordConfirm, passwordSaved = False }, Cmd.none, NoSessionChange )

        ChangePasswordClicked accessToken ->
            if model.newPassword /= model.newPasswordConfirm then
                ( { model | passwordError = Just "Passwords do not match" }, Cmd.none, NoSessionChange )

            else
                ( { model | passwordError = Nothing }
                , updatePassword accessToken model.newPassword
                , NoSessionChange
                )

        GotUpdatePasswordResponse (Ok _) ->
            ( { model
                | passwordError = Nothing
                , passwordSaved = True
                , newPassword = ""
                , newPasswordConfirm = ""
              }
            , Cmd.none
            , NoSessionChange
            )

        GotUpdatePasswordResponse (Err message) ->
            ( { model | passwordError = Just message, passwordSaved = False }, Cmd.none, NoSessionChange )

        NewEmailChanged newEmail ->
            ( { model | newEmail = newEmail, emailSaved = False }, Cmd.none, NoSessionChange )

        ChangeEmailClicked accessToken ->
            ( { model | emailError = Nothing }, updateEmail accessToken model.newEmail, NoSessionChange )

        GotUpdateEmailResponse (Ok _) ->
            ( { model | emailError = Nothing, emailSaved = True, newEmail = "" }, Cmd.none, NoSessionChange )

        GotUpdateEmailResponse (Err message) ->
            ( { model | emailError = Just message, emailSaved = False }, Cmd.none, NoSessionChange )

        LogoutClicked ->
            ( init, Cmd.none, SessionCleared )


updatePassword : String -> String -> Cmd Msg
updatePassword accessToken newPassword =
    authRequest
        { method = "PUT"
        , path = "/user"
        , accessToken = Just accessToken
        , body = Encode.object [ ( "password", Encode.string newPassword ) ]
        , expect = Http.expectStringResponse GotUpdatePasswordResponse handleUnitResponse
        }


updateEmail : String -> String -> Cmd Msg
updateEmail accessToken newEmail =
    authRequest
        { method = "PUT"
        , path = "/user"
        , accessToken = Just accessToken
        , body = Encode.object [ ( "email", Encode.string newEmail ) ]
        , expect = Http.expectStringResponse GotUpdateEmailResponse handleUnitResponse
        }


view : Session -> Model -> Html Msg
view session model =
    div [ class "account-page" ]
        [ accountSection "Account"
            [ div [ class "account-logged-in" ]
                [ span [ class "account-muted" ] [ text session.email ]
                , button [ class "button-ghost", onClick LogoutClicked ] [ text "Log out" ]
                ]
            ]
        , accountSection "Email" [ changeEmailForm session model ]
        , accountSection "Password" [ changePasswordForm session model ]
        ]


accountSection : String -> List (Html Msg) -> Html Msg
accountSection title fields =
    div [ class "settings-section" ]
        [ h3 [ class "history-heading" ] [ text title ]
        , div [ class "settings-fields" ] fields
        ]


changeEmailForm : Session -> Model -> Html Msg
changeEmailForm session model =
    div [ class "settings-field" ]
        [ form
            [ class "settings-inline-field"
            , onSubmit (ChangeEmailClicked session.accessToken)
            ]
            [ input
                [ class "auth-input"
                , type_ "email"
                , placeholder "New email"
                , value model.newEmail
                , onInput NewEmailChanged
                , required True
                ]
                []
            , button [ class "button-primary", type_ "submit" ] [ text "Update" ]
            ]
        , errorMessage "account-error" model.emailError
        , if model.emailSaved then
            p [ class "account-hint" ] [ text "Check your new email to confirm the change" ]

          else
            text ""
        ]


changePasswordForm : Session -> Model -> Html Msg
changePasswordForm session model =
    div [ class "settings-field" ]
        [ form
            [ class "settings-inline-field"
            , onSubmit (ChangePasswordClicked session.accessToken)
            ]
            [ div [ class "settings-inline-field-inputs" ]
                [ input
                    [ class "auth-input"
                    , type_ "password"
                    , placeholder "New password"
                    , value model.newPassword
                    , onInput NewPasswordChanged
                    , required True
                    ]
                    []
                , input
                    [ class "auth-input"
                    , type_ "password"
                    , placeholder "Confirm new password"
                    , value model.newPasswordConfirm
                    , onInput NewPasswordConfirmChanged
                    , required True
                    ]
                    []
                ]
            , button [ class "button-primary", type_ "submit" ] [ text "Update" ]
            ]
        , errorMessage "account-error" model.passwordError
        , if model.passwordSaved then
            p [ class "account-hint" ] [ text "Password updated" ]

          else
            text ""
        ]
