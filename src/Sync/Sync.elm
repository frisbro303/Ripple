module Sync.Sync exposing (appendOps, fetchOpIds, fetchOps)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ops.Op as Op
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sync.Config exposing (anonKey, supabaseUrl)
import Sync.Session exposing (Session)


opsLogRequest : Session -> { method : String, path : String, extraHeaders : List Http.Header, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
opsLogRequest session { method, path, extraHeaders, body, expect } =
    Http.request
        { method = method
        , headers =
            Http.header "apikey" anonKey
                :: Http.header "Authorization" ("Bearer " ++ session.accessToken)
                :: extraHeaders
        , url = supabaseUrl ++ "/rest/v1/ops_log" ++ path
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


fetchOps : Session -> (Result Http.Error OpsLog -> msg) -> Cmd msg
fetchOps session toMsg =
    opsLogRequest session
        { method = "GET"
        , path = "?select=*"
        , extraHeaders = []
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.map OpsLog.fromList (Decode.list Op.decoder))
        }


fetchOpIds : Session -> (Result Http.Error (List String) -> msg) -> Cmd msg
fetchOpIds session toMsg =
    opsLogRequest session
        { method = "GET"
        , path = "?select=id"
        , extraHeaders = []
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list (Decode.field "id" Decode.string))
        }


appendOps : Session -> OpsLog -> (Result Http.Error () -> msg) -> Cmd msg
appendOps session ops toMsg =
    opsLogRequest session
        { method = "POST"
        , path = ""
        , extraHeaders = [ Http.header "Prefer" "resolution=ignore-duplicates" ]
        , body = Http.jsonBody (Encode.list Op.encoder (OpsLog.toList ops))
        , expect = Http.expectWhatever toMsg
        }
