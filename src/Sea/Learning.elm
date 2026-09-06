module Sea.Learning exposing (Outcome(..), advance, initialStep)

import Sea.FSRS exposing (Rating(..))
import Time exposing (Posix)


{-| Number of consecutive Good ratings needed to graduate a card out of the
learning queue and into FSRS-6 scheduling. Cards resurface immediately
(not after some fixed delay) as long as they're still learning, rather than
making you wait a fixed number of minutes — a graduated card already can't
come back same-day (`nextIntervalDays` floors at 1 day), so the only thing
this queue is protecting against is FSRS-6's stability model, which is fit
on day-scale gaps and has no opinion on sub-day scheduling.
-}
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
