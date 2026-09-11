module Sea.Card exposing (Card, CardId, defer, isDue, new, resetToLearning, review)

import Sea.FSRS as FSRS exposing (Rating)
import Sea.Learning as Learning
import Time exposing (Posix)
import UUID exposing (UUID)


type alias CardId =
    UUID


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


resetToLearning : Posix -> Card -> Card
resetToLearning now card =
    { card
        | fsrs = FSRS.initialState now
        , learningStep = Just Learning.initialStep
    }


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
