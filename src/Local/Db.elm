port module Local.Db exposing (clearOps, insertLocalOp, insertLocalOps, insertSyncedOps, markSynced, opsLoaded, pendingOpsLoaded, requestOps, requestPendingOps)

import Json.Decode as Decode
import Json.Encode as Encode
import Ops.Op as Op exposing (Op)
import Ops.OpsLog as OpsLog exposing (OpsLog)


port insertOpsPort : { ops : Encode.Value, synced : Bool } -> Cmd msg


port markSyncedPort : List String -> Cmd msg


port requestOpsPort : () -> Cmd msg


port opsLoadedPort : (Decode.Value -> msg) -> Sub msg


port requestPendingOpsPort : () -> Cmd msg


port pendingOpsLoadedPort : (Decode.Value -> msg) -> Sub msg


port clearOpsPort : () -> Cmd msg


insertLocalOp : Op -> Cmd msg
insertLocalOp op =
    insertOpsPort { ops = Encode.list Op.encoder [ op ], synced = False }


insertLocalOps : OpsLog -> Cmd msg
insertLocalOps ops =
    insertOpsPort { ops = Encode.list Op.encoder (OpsLog.toList ops), synced = False }


insertSyncedOps : OpsLog -> Cmd msg
insertSyncedOps ops =
    insertOpsPort { ops = Encode.list Op.encoder (OpsLog.toList ops), synced = True }


markSynced : OpsLog -> Cmd msg
markSynced ops =
    markSyncedPort (List.map Op.idString (OpsLog.toList ops))


requestOps : Cmd msg
requestOps =
    requestOpsPort ()


opsLoaded : (Result Decode.Error OpsLog -> msg) -> Sub msg
opsLoaded toMsg =
    opsLoadedPort (decodeOpsLog >> toMsg)


requestPendingOps : Cmd msg
requestPendingOps =
    requestPendingOpsPort ()


pendingOpsLoaded : (Result Decode.Error OpsLog -> msg) -> Sub msg
pendingOpsLoaded toMsg =
    pendingOpsLoadedPort (decodeOpsLog >> toMsg)


decodeOpsLog : Decode.Value -> Result Decode.Error OpsLog
decodeOpsLog value =
    Decode.decodeValue (Decode.list Op.decoder) value
        |> Result.map OpsLog.fromList


clearOps : Cmd msg
clearOps =
    clearOpsPort ()
