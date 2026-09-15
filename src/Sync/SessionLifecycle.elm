module Sync.SessionLifecycle exposing (apply)

import Sync.LocalOps as LocalOps
import Sync.Session as Session exposing (Session, SessionUpdate(..))


apply :
    SessionUpdate
    -> { model | session : Maybe Session, localOps : LocalOps.Model }
    -> ( { model | session : Maybe Session, localOps : LocalOps.Model }, Cmd LocalOps.Msg )
apply sessionUpdate model =
    case sessionUpdate of
        SessionEstablished session ->
            ( { model | session = Just session }
            , Cmd.batch
                [ Session.save session
                , LocalOps.sessionEstablished session model.localOps
                ]
            )

        SessionCleared ->
            let
                ( localOpsModel, localOpsCmd ) =
                    LocalOps.sessionCleared
            in
            ( { model | session = Nothing, localOps = localOpsModel }
            , Cmd.batch [ Session.clear, localOpsCmd ]
            )

        NoSessionChange ->
            ( model, Cmd.none )
