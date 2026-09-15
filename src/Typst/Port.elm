port module Typst.Port exposing
    ( applyEdit
    , blurField
    , compileTypst
    , focusField
    , highlightTypst
    , setSelection
    , typstCompiled
    , typstHighlighted
    , windowFocusChanged
    )

import Json.Decode as Decode
import Typst.Keymap as Keymap


port focusField : String -> Cmd msg


port blurField : String -> Cmd msg


port windowFocusChanged : (Bool -> msg) -> Sub msg


port setSelectionPort : { id : String, start : Int, end : Int } -> Cmd msg


setSelection : String -> Int -> Int -> Cmd msg
setSelection id start end =
    setSelectionPort { id = id, start = start, end = end }


port applyEditPort :
    { id : String
    , rangeStart : Int
    , rangeEnd : Int
    , replacement : String
    , cursorStart : Int
    , cursorEnd : Int
    }
    -> Cmd msg


applyEdit : String -> Keymap.Edit -> Cmd msg
applyEdit id edit =
    applyEditPort
        { id = id
        , rangeStart = edit.rangeStart
        , rangeEnd = edit.rangeEnd
        , replacement = edit.replacement
        , cursorStart = edit.cursorStart
        , cursorEnd = edit.cursorEnd
        }


port compileTypstPort : { requestId : String, source : String, preamble : String, images : List ( String, String ) } -> Cmd msg


port rawTypstCompiledPort : (( String, Int, String ) -> msg) -> Sub msg


compileTypst : String -> String -> String -> List ( String, String ) -> Cmd msg
compileTypst requestId preamble source images =
    compileTypstPort { requestId = requestId, source = source, preamble = preamble, images = images }


typstCompiled : (String -> Result String String -> msg) -> Sub msg
typstCompiled toMsg =
    rawTypstCompiledPort
        (\( requestId, status, output ) ->
            if status == 0 then
                toMsg requestId (Ok output)

            else
                toMsg requestId (Err output)
        )


port highlightTypstPort : ( String, String ) -> Cmd msg


port typstHighlightedPort : (( String, Decode.Value ) -> msg) -> Sub msg


highlightTypst : String -> String -> Cmd msg
highlightTypst requestId source =
    highlightTypstPort ( requestId, source )


typstHighlighted : (String -> Decode.Value -> msg) -> Sub msg
typstHighlighted toMsg =
    typstHighlightedPort (\( requestId, tree ) -> toMsg requestId tree)
