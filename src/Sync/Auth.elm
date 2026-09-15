module Sync.Auth exposing (Model, Msg, authRequest, errorMessage, handleUnitResponse, init, refresh, update, view)

import Html exposing (Html, button, div, form, input, p, text)
import Html.Attributes exposing (class, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Sync.Config exposing (anonKey, supabaseUrl)
import Sync.Session as Session exposing (Session, SessionUpdate(..))


type Stage
    = EnteringCredentials
    | AwaitingVerification
    | AwaitingRecoveryCode


type AuthMode
    = Login
    | Signup
    | ForgotPassword


type LoginError
    = UnconfirmedEmail
    | AuthRejected String
    | OtherLoginError String


type alias Model =
    { email : String
    , password : String
    , confirmPassword : String
    , code : String
    , stage : Stage
    , mode : AuthMode
    , error : Maybe String
    }


type Msg
    = EmailChanged String
    | PasswordChanged String
    | ConfirmPasswordChanged String
    | CodeChanged String
    | ModeToggled
    | LoginClicked
    | GotLoginResponse (Result LoginError Session)
    | GotRefreshResponse (Result LoginError Session)
    | SignupClicked
    | GotSignupResponse (Result String ())
    | VerifyClicked
    | GotVerifyResponse (Result Http.Error Session)
    | ResendClicked
    | GotResendResponse (Result String ())
    | ForgotPasswordClicked
    | BackToLoginClicked
    | SendRecoveryClicked
    | GotRecoverResponse (Result String ())
    | RecoveryCodeVerifyClicked
    | GotRecoveryVerifyResponse (Result Http.Error Session)


init : Model
init =
    { email = ""
    , password = ""
    , confirmPassword = ""
    , code = ""
    , stage = EnteringCredentials
    , mode = Login
    , error = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg, SessionUpdate )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, Cmd.none, NoSessionChange )

        PasswordChanged password ->
            ( { model | password = password }, Cmd.none, NoSessionChange )

        ConfirmPasswordChanged confirmPassword ->
            ( { model | confirmPassword = confirmPassword }, Cmd.none, NoSessionChange )

        CodeChanged code ->
            ( { model | code = code }, Cmd.none, NoSessionChange )

        ModeToggled ->
            ( { model
                | mode =
                    if model.mode == Login then
                        Signup

                    else
                        Login
                , error = Nothing
              }
            , Cmd.none
            , NoSessionChange
            )

        LoginClicked ->
            ( { model | error = Nothing }, login model.email model.password, NoSessionChange )

        GotLoginResponse (Ok session) ->
            ( { model | error = Nothing }, Cmd.none, SessionEstablished session )

        GotLoginResponse (Err UnconfirmedEmail) ->
            ( { model | stage = AwaitingVerification, error = Nothing }
            , resend model.email
            , NoSessionChange
            )

        GotLoginResponse (Err (AuthRejected message)) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        GotLoginResponse (Err (OtherLoginError message)) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        GotRefreshResponse (Ok session) ->
            ( { model | error = Nothing }, Cmd.none, SessionEstablished session )

        GotRefreshResponse (Err (AuthRejected _)) ->
            ( init, Cmd.none, SessionCleared )

        GotRefreshResponse (Err UnconfirmedEmail) ->
            ( model, Cmd.none, NoSessionChange )

        GotRefreshResponse (Err (OtherLoginError _)) ->
            ( model, Cmd.none, NoSessionChange )

        SignupClicked ->
            if model.password /= model.confirmPassword then
                ( { model | error = Just "Passwords do not match" }, Cmd.none, NoSessionChange )

            else
                ( { model | error = Nothing }, signup model.email model.password, NoSessionChange )

        GotSignupResponse (Ok ()) ->
            ( { model | stage = AwaitingVerification, error = Nothing }, Cmd.none, NoSessionChange )

        GotSignupResponse (Err message) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        VerifyClicked ->
            ( { model | error = Nothing }, verify model.email model.code, NoSessionChange )

        GotVerifyResponse (Ok session) ->
            ( { model | error = Nothing }, Cmd.none, SessionEstablished session )

        GotVerifyResponse (Err _) ->
            ( { model | error = Just "Invalid or expired code" }, Cmd.none, NoSessionChange )

        ResendClicked ->
            ( { model | error = Nothing }, resend model.email, NoSessionChange )

        GotResendResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none, NoSessionChange )

        GotResendResponse (Err message) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        ForgotPasswordClicked ->
            ( { model | mode = ForgotPassword, error = Nothing }, Cmd.none, NoSessionChange )

        BackToLoginClicked ->
            ( { model | mode = Login, stage = EnteringCredentials, error = Nothing }, Cmd.none, NoSessionChange )

        SendRecoveryClicked ->
            ( { model | error = Nothing }, recover model.email, NoSessionChange )

        GotRecoverResponse (Ok _) ->
            ( { model | stage = AwaitingRecoveryCode, error = Nothing }, Cmd.none, NoSessionChange )

        GotRecoverResponse (Err message) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        RecoveryCodeVerifyClicked ->
            ( { model | error = Nothing }, verifyRecovery model.email model.code, NoSessionChange )

        GotRecoveryVerifyResponse (Ok session) ->
            ( { model | error = Nothing }, Cmd.none, SessionEstablished session )

        GotRecoveryVerifyResponse (Err _) ->
            ( { model | error = Just "Invalid or expired code" }, Cmd.none, NoSessionChange )


authRequest : { method : String, path : String, accessToken : Maybe String, body : Encode.Value, expect : Http.Expect msg } -> Cmd msg
authRequest { method, path, accessToken, body, expect } =
    Http.request
        { method = method
        , headers =
            Http.header "apikey" anonKey
                :: (accessToken
                        |> Maybe.map (\token -> [ Http.header "Authorization" ("Bearer " ++ token) ])
                        |> Maybe.withDefault []
                   )
        , url = supabaseUrl ++ "/auth/v1" ++ path
        , body = Http.jsonBody body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


login : String -> String -> Cmd Msg
login email password =
    authRequest
        { method = "POST"
        , path = "/token?grant_type=password"
        , accessToken = Nothing
        , body = Encode.object [ ( "email", Encode.string email ), ( "password", Encode.string password ) ]
        , expect = Http.expectStringResponse GotLoginResponse handleLoginResponse
        }


refresh : String -> Cmd Msg
refresh refreshToken =
    authRequest
        { method = "POST"
        , path = "/token?grant_type=refresh_token"
        , accessToken = Nothing
        , body = Encode.object [ ( "refresh_token", Encode.string refreshToken ) ]
        , expect = Http.expectStringResponse GotRefreshResponse handleLoginResponse
        }


type alias AuthErrorBody =
    { errorCode : String }


authErrorDecoder : Decode.Decoder AuthErrorBody
authErrorDecoder =
    Decode.map AuthErrorBody
        (Decode.field "error_code" Decode.string)


handleLoginResponse : Http.Response String -> Result LoginError Session
handleLoginResponse response =
    case response of
        Http.BadStatus_ metadata body ->
            case Decode.decodeString authErrorDecoder body of
                Ok { errorCode } ->
                    if errorCode == "email_not_confirmed" then
                        Err UnconfirmedEmail

                    else
                        Err (AuthRejected ("Login failed: " ++ errorCode))

                Err _ ->
                    Err (AuthRejected ("Login failed (" ++ String.fromInt metadata.statusCode ++ ")"))

        Http.GoodStatus_ _ body ->
            case Decode.decodeString Session.decoder body of
                Ok session ->
                    Ok session

                Err _ ->
                    Err (OtherLoginError "Unexpected response from server")

        Http.BadUrl_ _ ->
            Err (OtherLoginError "Bad URL")

        Http.Timeout_ ->
            Err (OtherLoginError "Request timed out")

        Http.NetworkError_ ->
            Err (OtherLoginError "Network error")


type alias ApiErrorBody =
    { errorCode : Maybe String
    , message : Maybe String
    }


apiErrorDecoder : Decode.Decoder ApiErrorBody
apiErrorDecoder =
    Decode.map2 ApiErrorBody
        (Decode.maybe (Decode.field "error_code" Decode.string))
        (Decode.maybe (Decode.field "msg" Decode.string))


handleUnitResponse : Http.Response String -> Result String ()
handleUnitResponse response =
    case response of
        Http.BadStatus_ metadata body ->
            case Decode.decodeString apiErrorDecoder body of
                Ok { errorCode, message } ->
                    Err
                        (message
                            |> orElse errorCode
                            |> Maybe.withDefault ("Request failed (" ++ String.fromInt metadata.statusCode ++ ")")
                        )

                Err _ ->
                    Err ("Request failed (" ++ String.fromInt metadata.statusCode ++ ")")

        Http.GoodStatus_ _ _ ->
            Ok ()

        Http.BadUrl_ _ ->
            Err "Bad URL"

        Http.Timeout_ ->
            Err "Request timed out"

        Http.NetworkError_ ->
            Err "Network error"


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback maybeValue =
    case maybeValue of
        Just value ->
            Just value

        Nothing ->
            fallback


signup : String -> String -> Cmd Msg
signup email password =
    authRequest
        { method = "POST"
        , path = "/signup"
        , accessToken = Nothing
        , body = Encode.object [ ( "email", Encode.string email ), ( "password", Encode.string password ) ]
        , expect = Http.expectStringResponse GotSignupResponse handleUnitResponse
        }


verify : String -> String -> Cmd Msg
verify email code =
    authRequest
        { method = "POST"
        , path = "/verify"
        , accessToken = Nothing
        , body =
            Encode.object
                [ ( "type", Encode.string "signup" )
                , ( "email", Encode.string email )
                , ( "token", Encode.string code )
                ]
        , expect = Http.expectJson GotVerifyResponse Session.decoder
        }


resend : String -> Cmd Msg
resend email =
    authRequest
        { method = "POST"
        , path = "/resend"
        , accessToken = Nothing
        , body = Encode.object [ ( "type", Encode.string "signup" ), ( "email", Encode.string email ) ]
        , expect = Http.expectStringResponse GotResendResponse handleUnitResponse
        }


recover : String -> Cmd Msg
recover email =
    authRequest
        { method = "POST"
        , path = "/recover"
        , accessToken = Nothing
        , body = Encode.object [ ( "email", Encode.string email ) ]
        , expect = Http.expectStringResponse GotRecoverResponse handleUnitResponse
        }


verifyRecovery : String -> String -> Cmd Msg
verifyRecovery email code =
    authRequest
        { method = "POST"
        , path = "/verify"
        , accessToken = Nothing
        , body =
            Encode.object
                [ ( "type", Encode.string "recovery" )
                , ( "email", Encode.string email )
                , ( "token", Encode.string code )
                ]
        , expect = Http.expectJson GotRecoveryVerifyResponse Session.decoder
        }


view : Model -> Html Msg
view model =
    div [ class "auth" ]
        [ case model.stage of
            EnteringCredentials ->
                case model.mode of
                    ForgotPassword ->
                        forgotPasswordForm model

                    _ ->
                        authForm model

            AwaitingVerification ->
                verifyForm model

            AwaitingRecoveryCode ->
                recoveryCodeForm model
        ]


authForm : Model -> Html Msg
authForm model =
    let
        isSignup =
            model.mode == Signup

        modeLabel =
            if isSignup then
                "Sign up"

            else
                "Log in"
    in
    div [ class "auth-card" ]
        [ p [ class "auth-title" ] [ text modeLabel ]
        , form
            [ class "auth-form"
            , onSubmit
                (if isSignup then
                    SignupClicked

                 else
                    LoginClicked
                )
            ]
            ([ input
                [ class "auth-input"
                , type_ "email"
                , placeholder "Email"
                , value model.email
                , onInput EmailChanged
                , required True
                ]
                []
             , input
                [ class "auth-input"
                , type_ "password"
                , placeholder "Password"
                , value model.password
                , onInput PasswordChanged
                , required True
                ]
                []
             ]
                ++ (if isSignup then
                        [ input
                            [ class "auth-input"
                            , type_ "password"
                            , placeholder "Confirm password"
                            , value model.confirmPassword
                            , onInput ConfirmPasswordChanged
                            , required True
                            ]
                            []
                        ]

                    else
                        []
                   )
                ++ [ errorMessage "auth-error" model.error
                   , button [ class "button-primary", type_ "submit" ] [ text modeLabel ]
                   ]
            )
        , button [ class "auth-switch", onClick ModeToggled ]
            [ text
                (if isSignup then
                    "Already have an account? Log in"

                 else
                    "Need an account? Sign up"
                )
            ]
        , if isSignup then
            text ""

          else
            button [ class "auth-switch", onClick ForgotPasswordClicked ] [ text "Forgot password?" ]
        ]


forgotPasswordForm : Model -> Html Msg
forgotPasswordForm model =
    div [ class "auth-card" ]
        [ p [ class "auth-title" ] [ text "Reset password" ]
        , p [ class "auth-hint" ] [ text "We'll email you a code to reset your password." ]
        , form [ class "auth-form", onSubmit SendRecoveryClicked ]
            [ input
                [ class "auth-input"
                , type_ "email"
                , placeholder "Email"
                , value model.email
                , onInput EmailChanged
                , required True
                ]
                []
            , errorMessage "auth-error" model.error
            , button [ class "button-primary", type_ "submit" ] [ text "Send reset code" ]
            ]
        , button [ class "auth-switch", onClick BackToLoginClicked ] [ text "Back to log in" ]
        ]


recoveryCodeForm : Model -> Html Msg
recoveryCodeForm model =
    div [ class "auth-card" ]
        [ p [ class "auth-title" ] [ text "Reset password" ]
        , p [ class "auth-hint" ] [ text ("Enter the code sent to " ++ model.email) ]
        , form [ class "auth-form", onSubmit RecoveryCodeVerifyClicked ]
            [ input
                [ class "auth-input"
                , type_ "text"
                , placeholder "Reset code"
                , value model.code
                , onInput CodeChanged
                , required True
                ]
                []
            , errorMessage "auth-error" model.error
            , button [ class "button-primary", type_ "submit" ] [ text "Verify" ]
            ]
        , button [ class "auth-switch", onClick SendRecoveryClicked ] [ text "Resend code" ]
        , button [ class "auth-switch", onClick BackToLoginClicked ] [ text "Back to log in" ]
        ]


verifyForm : Model -> Html Msg
verifyForm model =
    div [ class "auth-card" ]
        [ p [ class "auth-title" ] [ text "Confirm your email" ]
        , p [ class "auth-hint" ] [ text ("Enter the code sent to " ++ model.email) ]
        , form [ class "auth-form", onSubmit VerifyClicked ]
            [ input
                [ class "auth-input"
                , type_ "text"
                , placeholder "Verification code"
                , value model.code
                , onInput CodeChanged
                , required True
                ]
                []
            , errorMessage "auth-error" model.error
            , button [ class "button-primary", type_ "submit" ] [ text "Verify" ]
            ]
        , button [ class "auth-switch", onClick ResendClicked ] [ text "Resend code" ]
        ]


errorMessage : String -> Maybe String -> Html msg
errorMessage className error =
    case error of
        Just err ->
            p [ class className ] [ text err ]

        Nothing ->
            text ""
