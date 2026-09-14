module Typst.Editor.Resize exposing (Drag, defaultHeight, drag, start)


type alias Drag =
    { startY : Float
    , startHeight : Float
    }


defaultHeight : Float
defaultHeight =
    96


minHeight : Float
minHeight =
    64


maxHeight : Float
maxHeight =
    480


start : Float -> Float -> Drag
start clientY currentHeight =
    { startY = clientY, startHeight = currentHeight }


drag : Float -> Drag -> Float
drag clientY currentDrag =
    clamp minHeight maxHeight (currentDrag.startHeight + (clientY - currentDrag.startY))
