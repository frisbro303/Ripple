module Sync.LocalOps exposing (Model, Msg(..), SyncStatus(..), init, insertNewOp, requestSync, sessionCleared, sessionEstablished, subscriptions, update)

import Http
import Json.Decode as Decode
import Local.Db as Db
import Ops.Op exposing (Op)
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Set
import Sync.Session exposing (Session)
import Sync.Sync as Sync
import Time


type alias Model =
    OpsLog


type Msg
    = LocalOpsLoaded (Result Decode.Error OpsLog)
    | SyncTick
    | GotRemoteOpIds (Result Http.Error (List String))
    | GotRemoteOps (Result Http.Error OpsLog)
    | GotPushResult (Result Http.Error ())
    | ImportedOps OpsLog


syncIntervalMs : Float
syncIntervalMs =
    15000


init : ( Model, Cmd Msg )
init =
    ( OpsLog.emptyOpsLog, Db.requestOps )


insertNewOp : Op -> Model -> ( Model, Cmd Msg )
insertNewOp op model =
    ( OpsLog.insert op model, Db.insertOp op )


sessionEstablished : Session -> Cmd Msg
sessionEstablished session =
    Sync.fetchOps session GotRemoteOps


sessionCleared : ( Model, Cmd Msg )
sessionCleared =
    ( OpsLog.emptyOpsLog, Db.clearOps )


type SyncStatus
    = NoStatusChange
    | SyncFailed String
    | SessionExpired
    | SyncSucceeded


update : Maybe Session -> Msg -> Model -> ( Model, Cmd Msg, SyncStatus )
update session msg model =
    case msg of
        LocalOpsLoaded (Ok ops) ->
            ( OpsLog.merge ops model, Cmd.none, NoStatusChange )

        LocalOpsLoaded (Err error) ->
            ( model, Cmd.none, SyncFailed ("Couldn't load saved cards: " ++ Decode.errorToString error) )

        SyncTick ->
            ( model, requestSync session, NoStatusChange )

        GotRemoteOpIds (Ok remoteIds) ->
            if Set.fromList remoteIds == OpsLog.idStrings model then
                ( model, Cmd.none, SyncSucceeded )

            else
                case session of
                    Just activeSession ->
                        ( model, Sync.fetchOps activeSession GotRemoteOps, NoStatusChange )

                    Nothing ->
                        ( model, Cmd.none, NoStatusChange )

        GotRemoteOpIds (Err error) ->
            ( model, Cmd.none, classifyError error )

        GotRemoteOps (Ok remoteOps) ->
            case session of
                Just activeSession ->
                    let
                        toPull =
                            OpsLog.diff remoteOps model

                        toPush =
                            OpsLog.diff model remoteOps
                    in
                    ( OpsLog.merge remoteOps model
                    , Cmd.batch
                        [ Db.insertOps toPull
                        , Sync.appendOps activeSession toPush GotPushResult
                        ]
                    , SyncSucceeded
                    )

                Nothing ->
                    ( model, Cmd.none, NoStatusChange )

        GotRemoteOps (Err error) ->
            ( model, Cmd.none, classifyError error )

        GotPushResult (Ok ()) ->
            ( model, Cmd.none, SyncSucceeded )

        GotPushResult (Err error) ->
            ( model, Cmd.none, classifyError error )

        ImportedOps importedOps ->
            let
                toInsert =
                    OpsLog.diff importedOps model
            in
            ( OpsLog.merge importedOps model, Db.insertOps toInsert, NoStatusChange )


classifyError : Http.Error -> SyncStatus
classifyError error =
    case error of
        Http.BadStatus 401 ->
            SessionExpired

        _ ->
            SyncFailed (describeHttpError error)


describeHttpError : Http.Error -> String
describeHttpError error =
    case error of
        Http.BadUrl _ ->
            "Sync failed: bad URL"

        Http.Timeout ->
            "Sync timed out"

        Http.NetworkError ->
            "Sync failed: no network connection"

        Http.BadStatus code ->
            "Sync failed (" ++ String.fromInt code ++ ")"

        Http.BadBody _ ->
            "Sync failed: unexpected response"


requestSync : Maybe Session -> Cmd Msg
requestSync maybeSession =
    case maybeSession of
        Just session ->
            Sync.fetchOpIds session GotRemoteOpIds

        Nothing ->
            Cmd.none


subscriptions : Maybe Session -> Sub Msg
subscriptions session =
    Sub.batch
        [ Db.opsLoaded LocalOpsLoaded
        , case session of
            Just _ ->
                Time.every syncIntervalMs (always SyncTick)

            Nothing ->
                Sub.none
        ]
