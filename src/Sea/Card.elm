module Sea.Card exposing (..)

import UUID exposing (UUID)
import Time exposing (Posix)

import Sea.FSRS as FSRS exposing (Rating)
import Sea.Learning as Learning


type alias CardId =
    UUID


{-| `learningStep` tracks a card's position in the short-term learning queue
(see `Sea.Learning`) — `Just n` means the card hasn't graduated into FSRS-6
scheduling yet and is on step `n`; `Nothing` means it's a normal FSRS-scheduled
card. While learning, `fsrs.due` holds the next same-session re-show time and
`fsrs.stability`/`fsrs.difficulty` stay at their new-card zero value, so
`FSRS.isNew` (and everything keyed off it, like the daily new-card limit)
continues to treat the card as new until it graduates.
-}
type alias Card =
    { id : CardId
    , front : String
    , back : String
    , fsrs : FSRS.State
    , learningStep : Maybe Int
    }


new : CardId -> String -> String -> Posix -> Card
new id front back now =
    { id = id
    , front = front
    , back = back
    , fsrs = FSRS.initialState now
    , learningStep = Just Learning.initialStep
    }


review : Float -> Posix -> Rating -> Card -> Card
review desiredRetention now rating card =
    case card.learningStep of
        Just currentStep ->
            case Learning.advance now rating currentStep of
                Learning.StillLearning { step, due } ->
                    { card
                        | learningStep = Just step
                        , fsrs = setFsrsDue now due card.fsrs
                    }

                Learning.Graduated ->
                    { card
                        | learningStep = Nothing
                        , fsrs = FSRS.review desiredRetention now rating card.fsrs
                    }

        Nothing ->
            { card | fsrs = FSRS.review desiredRetention now rating card.fsrs }


{-| Wipes a card's FSRS progress and puts it back at the front of the
learning queue, due immediately — same starting state as a freshly created
card, just keeping its id/front/back.
-}
resetToLearning : Posix -> Card -> Card
resetToLearning now card =
    { card
        | fsrs = FSRS.initialState now
        , learningStep = Just Learning.initialStep
    }


{-| Pushes a card's next appearance out by `days` from now, without
touching its scheduling state (stability/difficulty/learning step) —
a pure "not today" postponement, not a review.
-}
defer : Int -> Posix -> Card -> Card
defer days now card =
    let
        fsrs =
            card.fsrs
    in
    { card | fsrs = { fsrs | due = addDays days now } }


addDays : Int -> Posix -> Posix
addDays days t =
    Time.millisToPosix (Time.posixToMillis t + days * 86400000)


setFsrsDue : Posix -> Posix -> FSRS.State -> FSRS.State
setFsrsDue now due fsrs =
    { fsrs | due = due, lastReview = now }

isDue : Posix -> Card -> Bool
isDue now card =
    Time.posixToMillis card.fsrs.due
        <= Time.posixToMillis now
