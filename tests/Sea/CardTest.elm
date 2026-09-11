module Sea.CardTest exposing (suite)

import Expect
import Random
import Sea.Card as Card
import Sea.FSRS exposing (Rating(..))
import Test exposing (Test, describe, test)
import Time
import UUID exposing (UUID)


cardId : UUID
cardId =
    Random.step UUID.generator (Random.initialSeed 1) |> Tuple.first


now : Time.Posix
now =
    Time.millisToPosix 1000000


suite : Test
suite =
    describe "Sea.Card"
        [ test "a freshly created card is due immediately" <|
            \_ ->
                Card.new cardId "front" "back" now
                    |> Card.isDue now
                    |> Expect.equal True
        , test "deferring pushes the due date into the future, out of range for `now`" <|
            \_ ->
                Card.new cardId "front" "back" now
                    |> Card.defer 2 now
                    |> Card.isDue now
                    |> Expect.equal False
        , test "resetToLearning clears prior review progress back to a fresh card's state" <|
            \_ ->
                let
                    reviewed =
                        Card.new cardId "front" "back" now
                            |> Card.review 0.9 now Good

                    reset =
                        Card.resetToLearning now reviewed
                in
                Expect.all
                    [ \c -> c.fsrs.stability |> Expect.equal 0
                    , \c -> c.fsrs.difficulty |> Expect.equal 0
                    , \c -> Card.isDue now c |> Expect.equal True
                    ]
                    reset
        , test "front/back text survives a review" <|
            \_ ->
                Card.new cardId "front" "back" now
                    |> Card.review 0.9 now Good
                    |> (\c -> ( c.front, c.back ))
                    |> Expect.equal ( "front", "back" )
        ]
