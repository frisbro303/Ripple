module Sea.Learning exposing (Outcome(..), advance, initialStep)

import Sea.FSRS exposing (Rating(..))
import Time exposing (Posix)


totalSteps : Int
totalSteps =
    2


initialStep : Int
initialStep =
    0


type Outcome
    = StillLearning { step : Int, due : Posix }
    | Graduated


advance : Posix -> Rating -> Int -> Outcome
advance now rating step =
    case rating of
        Easy ->
            Graduated

        Again ->
            StillLearning { step = initialStep, due = now }

        Hard ->
            StillLearning { step = step, due = now }

        Good ->
            let
                nextStep =
                    step + 1
            in
            if nextStep >= totalSteps then
                Graduated

            else
                StillLearning { step = nextStep, due = now }
