module Typst.Keymap exposing (Edit, KeyInfo, Outcome(..), classify, onKeyDown)

import Html
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import List.Extra


type alias KeyInfo =
    { key : String
    , shift : Bool
    , value : String
    , start : Int
    , end : Int
    }


type Outcome
    = PassThrough
    | Blur
    | Chain
    | Replace Edit


type alias Edit =
    { rangeStart : Int
    , rangeEnd : Int
    , replacement : String
    , cursorStart : Int
    , cursorEnd : Int
    }


onKeyDown : Bool -> (Outcome -> msg) -> Html.Attribute msg
onKeyDown chainable toMsg =
    preventDefaultOn "keydown"
        (infoDecoder
            |> Decode.map (classify chainable)
            |> Decode.andThen
                (\outcome ->
                    case outcome of
                        PassThrough ->
                            Decode.fail "not intercepted"

                        _ ->
                            Decode.succeed ( toMsg outcome, True )
                )
        )


infoDecoder : Decode.Decoder KeyInfo
infoDecoder =
    Decode.map5 KeyInfo
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.at [ "target", "value" ] Decode.string)
        (Decode.at [ "target", "selectionStart" ] Decode.int)
        (Decode.at [ "target", "selectionEnd" ] Decode.int)


classify : Bool -> KeyInfo -> Outcome
classify chainable info =
    if info.key == "Escape" then
        Blur

    else if info.key == "Enter" && info.shift && chainable then
        Chain

    else
        case openToClose info.key of
            Just close ->
                bracketOrFence info.key close False info

            Nothing ->
                case fenceClose info.key of
                    Just close ->
                        bracketOrFence info.key close True info

                    Nothing ->
                        classifyOther info


classifyOther : KeyInfo -> Outcome
classifyOther info =
    if isCloseChar info.key && info.start == info.end && charAt info.start info.value == Just info.key then
        skipOver info.start

    else if info.key == "Backspace" && info.start == info.end && info.start > 0 then
        backspaceAction info

    else if info.key == "Tab" then
        Just (tabAction info) |> Maybe.withDefault PassThrough

    else if info.key == "Enter" && info.start == info.end then
        enterAction info

    else
        PassThrough


skipOver : Int -> Outcome
skipOver pos =
    Replace { rangeStart = pos, rangeEnd = pos, replacement = "", cursorStart = pos + 1, cursorEnd = pos + 1 }


bracketOrFence : String -> String -> Bool -> KeyInfo -> Outcome
bracketOrFence key close isFence info =
    if info.start /= info.end then
        let
            selected =
                String.slice info.start info.end info.value
        in
        Replace
            { rangeStart = info.start
            , rangeEnd = info.end
            , replacement = key ++ selected ++ close
            , cursorStart = info.start + 1
            , cursorEnd = info.start + 1 + String.length selected
            }

    else if isFence && charAt info.start info.value == Just key then
        skipOver info.start

    else
        Replace
            { rangeStart = info.start
            , rangeEnd = info.start
            , replacement = key ++ close
            , cursorStart = info.start + 1
            , cursorEnd = info.start + 1
            }


backspaceAction : KeyInfo -> Outcome
backspaceAction info =
    let
        before =
            charAt (info.start - 1) info.value

        after =
            charAt info.start info.value
    in
    if matchesPair before after then
        Replace
            { rangeStart = info.start - 1
            , rangeEnd = info.start + 1
            , replacement = ""
            , cursorStart = info.start - 1
            , cursorEnd = info.start - 1
            }

    else
        PassThrough


tabAction : KeyInfo -> Outcome
tabAction info =
    if info.start /= info.end && String.contains "\n" (String.slice info.start info.end info.value) then
        let
            lineStart =
                lastNewlineBefore info.start info.value + 1

            selected =
                String.slice lineStart info.end info.value

            newSelected =
                String.split "\n" selected
                    |> List.map
                        (if info.shift then
                            stripUpTo2Spaces

                         else
                            (++) "  "
                        )
                    |> String.join "\n"
        in
        Replace
            { rangeStart = lineStart
            , rangeEnd = info.end
            , replacement = newSelected
            , cursorStart = lineStart
            , cursorEnd = lineStart + String.length newSelected
            }

    else
        Replace
            { rangeStart = info.start
            , rangeEnd = info.end
            , replacement = "  "
            , cursorStart = info.start + 2
            , cursorEnd = info.start + 2
            }


enterAction : KeyInfo -> Outcome
enterAction info =
    let
        indent =
            currentLineIndent info.value info.start

        before =
            charAt (info.start - 1) info.value

        after =
            charAt info.start info.value
    in
    if matchesPair before after then
        let
            innerIndent =
                indent ++ "  "

            insertion =
                "\n" ++ innerIndent ++ "\n" ++ indent

            newPos =
                info.start + 1 + String.length innerIndent
        in
        Replace
            { rangeStart = info.start
            , rangeEnd = info.start
            , replacement = insertion
            , cursorStart = newPos
            , cursorEnd = newPos
            }

    else if indent /= "" then
        let
            insertion =
                "\n" ++ indent

            newPos =
                info.start + String.length insertion
        in
        Replace
            { rangeStart = info.start
            , rangeEnd = info.start
            , replacement = insertion
            , cursorStart = newPos
            , cursorEnd = newPos
            }

    else
        PassThrough


openToClose : String -> Maybe String
openToClose key =
    case key of
        "(" ->
            Just ")"

        "[" ->
            Just "]"

        "{" ->
            Just "}"

        _ ->
            Nothing


fenceClose : String -> Maybe String
fenceClose key =
    case key of
        "\"" ->
            Just "\""

        "`" ->
            Just "`"

        "$" ->
            Just "$"

        _ ->
            Nothing


isCloseChar : String -> Bool
isCloseChar key =
    List.member key [ ")", "]", "}" ]


matchesPair : Maybe String -> Maybe String -> Bool
matchesPair before after =
    case before |> Maybe.andThen openToClose of
        Just expected ->
            after == Just expected

        Nothing ->
            case before |> Maybe.andThen fenceClose of
                Just expected ->
                    after == Just expected

                Nothing ->
                    False


charAt : Int -> String -> Maybe String
charAt i s =
    case String.slice i (i + 1) s of
        "" ->
            Nothing

        c ->
            Just c


lastNewlineBefore : Int -> String -> Int
lastNewlineBefore pos value =
    String.indexes "\n" (String.left pos value)
        |> List.maximum
        |> Maybe.withDefault -1


currentLineIndent : String -> Int -> String
currentLineIndent value pos =
    let
        lineStart =
            lastNewlineBefore pos value + 1
    in
    String.slice lineStart pos value
        |> String.toList
        |> List.Extra.takeWhile (\c -> c == ' ' || c == '\t')
        |> String.fromList


stripUpTo2Spaces : String -> String
stripUpTo2Spaces line =
    if String.startsWith "  " line then
        String.dropLeft 2 line

    else if String.startsWith " " line then
        String.dropLeft 1 line

    else
        line
