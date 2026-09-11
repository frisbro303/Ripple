module ThemeTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Theme exposing (Theme(..))


suite : Test
suite =
    describe "Theme"
        [ test "toString/fromString round-trips for every variant" <|
            \_ ->
                [ System, Light, Dark ]
                    |> List.map (Theme.toString >> Theme.fromString)
                    |> Expect.equal [ System, Light, Dark ]
        , test "an unrecognized string falls back to System" <|
            \_ ->
                Theme.fromString "not-a-theme" |> Expect.equal System
        ]
