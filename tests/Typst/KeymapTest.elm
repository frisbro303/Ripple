module Typst.KeymapTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Typst.Keymap as Keymap exposing (Outcome(..))


info : String -> Bool -> String -> Int -> Int -> Keymap.KeyInfo
info key shift value start end =
    { key = key, shift = shift, value = value, start = start, end = end }


replace : Int -> Int -> String -> Int -> Int -> Outcome
replace rangeStart rangeEnd replacement cursorStart cursorEnd =
    Replace
        { rangeStart = rangeStart
        , rangeEnd = rangeEnd
        , replacement = replacement
        , cursorStart = cursorStart
        , cursorEnd = cursorEnd
        }


suite : Test
suite =
    describe "Typst.Keymap.classify"
        [ describe "bracket/quote pairing"
            [ test "typing ( with collapsed cursor inserts the pair and places the cursor between" <|
                \_ ->
                    Keymap.classify False (info "(" False "ab" 1 1)
                        |> Expect.equal (replace 1 1 "()" 2 2)
            , test "typing ( around a selection wraps it" <|
                \_ ->
                    Keymap.classify False (info "(" False "hello world" 0 5)
                        |> Expect.equal (replace 0 5 "(hello)" 1 6)
            , test "typing a quote right before an existing matching quote skips over it" <|
                \_ ->
                    Keymap.classify False (info "\"" False "\"foo\"" 4 4)
                        |> Expect.equal (replace 4 4 "" 5 5)
            , test "typing ( right before an existing ( does not skip over (nests instead)" <|
                \_ ->
                    Keymap.classify False (info "(" False "((" 1 1)
                        |> Expect.equal (replace 1 1 "()" 2 2)
            , test "typing an unrelated close char with no matching open just passes through" <|
                \_ ->
                    Keymap.classify False (info ")" False "abc" 1 1)
                        |> Expect.equal PassThrough
            ]
        , describe "close-char skip-over"
            [ test "typing ) right before an existing ) skips over it instead of inserting" <|
                \_ ->
                    Keymap.classify False (info ")" False "(foo)" 4 4)
                        |> Expect.equal (replace 4 4 "" 5 5)
            ]
        , describe "Backspace pair-deletion"
            [ test "backspacing between an adjacent pair deletes both" <|
                \_ ->
                    Keymap.classify False (info "Backspace" False "a()b" 2 2)
                        |> Expect.equal (replace 1 3 "" 1 1)
            , test "backspacing between non-pair chars passes through" <|
                \_ ->
                    Keymap.classify False (info "Backspace" False "abc" 2 2)
                        |> Expect.equal PassThrough
            ]
        , describe "Tab indent/outdent"
            [ test "Tab with collapsed cursor inserts two spaces" <|
                \_ ->
                    Keymap.classify False (info "Tab" False "ab" 1 1)
                        |> Expect.equal (replace 1 1 "  " 3 3)
            , test "Tab over a multi-line selection indents every line by two spaces" <|
                \_ ->
                    Keymap.classify False (info "Tab" False "foo\nbar" 0 7)
                        |> Expect.equal (replace 0 7 "  foo\n  bar" 0 11)
            , test "Shift+Tab over a multi-line selection outdents up to two leading spaces per line" <|
                \_ ->
                    Keymap.classify False (info "Tab" True "  foo\n    bar" 0 13)
                        |> Expect.equal (replace 0 13 "foo\n  bar" 0 9)
            ]
        , describe "Enter auto-indent"
            [ test "Enter on an indented line carries the indent forward" <|
                \_ ->
                    Keymap.classify False (info "Enter" False "  foo" 5 5)
                        |> Expect.equal (replace 5 5 "\n  " 8 8)
            , test "Enter with no indentation to carry passes through (native newline)" <|
                \_ ->
                    Keymap.classify False (info "Enter" False "foo" 3 3)
                        |> Expect.equal PassThrough
            , test "Enter between an adjacent pair expands onto three lines, indented" <|
                \_ ->
                    Keymap.classify False (info "Enter" False "{}" 1 1)
                        |> Expect.equal (replace 1 1 "\n  \n" 4 4)
            ]
        , describe "Escape"
            [ test "Escape blurs regardless of chainable" <|
                \_ ->
                    Keymap.classify True (info "Escape" False "abc" 1 1)
                        |> Expect.equal Blur
            ]
        , describe "Shift+Enter chaining"
            [ test "Shift+Enter chains when chainable" <|
                \_ ->
                    Keymap.classify True (info "Enter" True "abc" 1 1)
                        |> Expect.equal Chain
            , test "Shift+Enter behaves like plain Enter when not chainable" <|
                \_ ->
                    Keymap.classify False (info "Enter" True "  foo" 5 5)
                        |> Expect.equal (replace 5 5 "\n  " 8 8)
            ]
        , describe "plain typing"
            [ test "an ordinary letter passes through untouched" <|
                \_ ->
                    Keymap.classify False (info "a" False "bc" 1 1)
                        |> Expect.equal PassThrough
            ]
        ]
