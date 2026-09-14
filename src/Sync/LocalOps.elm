module Sync.LocalOps exposing (Model, Msg(..), SyncStatus(..), init, insertNewOp, requestSync, sessionCleared, sessionEstablished, subscriptions, update)

import Http
import Json.Decode as Decode
import Local.Db as Db
import Ops.Op exposing (Op)
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sync.Session exposing (Session)
import Sync.Sync as Sync
import Time


type alias Model =
    OpsLog


type Msg
    = LocalOpsLoaded (Result Decode.Error OpsLog)
    | SyncTick
    | GotRemoteOps (Result Http.Error OpsLog)
    | PendingOpsLoaded (Result Decode.Error OpsLog)
    | GotPushResult OpsLog (Result Http.Error ())
    | ImportedOps OpsLog


syncIntervalMs : Float
syncIntervalMs =
    15000


init : ( Model, Cmd Msg )
init =
    ( OpsLog.emptyOpsLog, Db.requestOps )


insertNewOp : Maybe Session -> Op -> Model -> ( Model, Cmd Msg )
insertNewOp session op model =
    ( OpsLog.insert op model
    , Cmd.batch [ Db.insertLocalOp op, pushNow session (OpsLog.fromList [ op ]) ]
    )


pushNow : Maybe Session -> OpsLog -> Cmd Msg
pushNow session ops =
    case session of
        Just activeSession ->
            Sync.appendOps activeSession ops (GotPushResult ops)

        Nothing ->
            Cmd.none


pull : Session -> Model -> Cmd Msg
pull session model =
    Sync.fetchOpsSince session (OpsLog.maxTimestamp model) GotRemoteOps


sessionEstablished : Session -> Model -> Cmd Msg
sessionEstablished session model =
    Cmd.batch [ pull session model, Db.requestPendingOps ]


requestSync : Maybe Session -> Model -> Cmd Msg
requestSync session model =
    case session of
        Just activeSession ->
            Cmd.batch [ pull activeSession model, Db.requestPendingOps ]

        Nothing ->
            Cmd.none


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
            ( model, requestSync session model, NoStatusChange )

        GotRemoteOps (Ok remoteOps) ->
            ( OpsLog.merge remoteOps model, Db.insertSyncedOps remoteOps, SyncSucceeded )

        GotRemoteOps (Err error) ->
            ( model, Cmd.none, classifyError error )

        PendingOpsLoaded (Ok pendingOps) ->
            ( model, pushNow session pendingOps, NoStatusChange )

        PendingOpsLoaded (Err error) ->
            ( model, Cmd.none, SyncFailed ("Couldn't read pending changes: " ++ Decode.errorToString error) )

        GotPushResult pushedOps (Ok ()) ->
            ( model, Db.markSynced pushedOps, SyncSucceeded )

        GotPushResult _ (Err error) ->
            ( model, Cmd.none, classifyError error )

        ImportedOps importedOps ->
            let
                toInsert =
                    OpsLog.diff importedOps model
            in
            ( OpsLog.merge importedOps model
            , Cmd.batch [ Db.insertLocalOps toInsert, pushNow session toInsert ]
            , NoStatusChange
            )


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


subscriptions : Maybe Session -> Sub Msg
subscriptions session =
    Sub.batch
        [ Db.opsLoaded LocalOpsLoaded
        , Db.pendingOpsLoaded PendingOpsLoaded
        , case session of
            Just _ ->
                Time.every syncIntervalMs (always SyncTick)

            Nothing ->
                Sub.none
        ]
