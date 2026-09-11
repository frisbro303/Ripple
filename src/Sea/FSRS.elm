module Sea.FSRS exposing
    ( Rating(..)
    , State
    , dateOf
    , defaultDesiredRetention
    , elapsedDays
    , initialState
    , isNew
    , retrievability
    , review
    )

import Array exposing (Array)
import Date exposing (Date)
import Time exposing (Posix)


type Rating
    = Again
    | Hard
    | Good
    | Easy


type alias State =
    { due : Posix
    , stability : Float
    , difficulty : Float
    , lastReview : Posix
    }


rolloverHour : Int
rolloverHour =
    4


dateOf : Posix -> Date
dateOf t =
    Date.fromPosix Time.utc (Time.millisToPosix (Time.posixToMillis t - rolloverHour * 3600000))


elapsedDays : Posix -> Posix -> Int
elapsedDays from to =
    Date.diff Date.Days (dateOf from) (dateOf to)


fsrsWeights : Array Float
fsrsWeights =
    Array.fromList
        [ 0.212
        , 1.2931
        , 2.3065
        , 8.2956
        , 6.4133
        , 0.8334
        , 3.0194
        , 0.001
        , 1.8722
        , 0.1666
        , 0.796
        , 1.4835
        , 0.0614
        , 0.2629
        , 1.6483
        , 0.6014
        , 1.8729
        , 0.5425
        , 0.0912
        , 0.0658
        , 0.1542
        ]


weight : Int -> Float
weight i =
    Array.get i fsrsWeights |> Maybe.withDefault 0


defaultDesiredRetention : Float
defaultDesiredRetention =
    0.9


decay =
    -(weight 20)


factor =
    0.9 ^ (1 / decay) - 1


ratingNumber rating =
    case rating of
        Again ->
            1

        Hard ->
            2

        Good ->
            3

        Easy ->
            4


retrievability elapsed stability =
    (1 + factor * max 0 elapsed / stability) ^ decay


initialStability rating =
    weight (round (ratingNumber rating) - 1)


rawInitialDifficulty rating =
    weight 4 - e ^ (weight 5 * (ratingNumber rating - 1)) + 1


initialDifficulty rating =
    clamp 1 10 (rawInitialDifficulty rating)


nextDifficulty difficulty rating =
    let
        damped =
            difficulty - weight 6 * (ratingNumber rating - 3) * (10 - difficulty) / 9
    in
    clamp 1 10 (weight 7 * rawInitialDifficulty Easy + (1 - weight 7) * damped)


shortTermStability stability rating =
    let
        multiplier =
            e ^ (weight 17 * (ratingNumber rating - 3 + weight 18)) * stability ^ -(weight 19)
    in
    stability
        * (if rating == Again then
            multiplier

           else
            max 1 multiplier
          )


recallStability difficulty stability r rating =
    let
        bonus =
            if rating == Hard then
                weight 15

            else if rating == Easy then
                weight 16

            else
                1
    in
    stability * (e ^ weight 8 * (11 - difficulty) * stability ^ -(weight 9) * (e ^ (weight 10 * (1 - r)) - 1) * bonus + 1)


forgetStability difficulty stability r =
    let
        newStability =
            weight 11 * difficulty ^ -(weight 12) * ((stability + 1) ^ weight 13 - 1) * e ^ (weight 14 * (1 - r))

        newStabilityMin =
            stability / e ^ (weight 17 * weight 18)
    in
    min newStability newStabilityMin


nextIntervalDays desiredRetention stability =
    max 1 ((stability / factor) * (desiredRetention ^ (1 / decay) - 1))


addDays days t =
    Time.millisToPosix (Time.posixToMillis t + round (days * 86400000))


initialState : Posix -> State
initialState now =
    { due = now, stability = 0, difficulty = 0, lastReview = now }


isNew state =
    state.difficulty == 0


review : Float -> Posix -> Rating -> State -> State
review desiredRetention now rating state =
    let
        ( newDifficulty, newStability ) =
            if isNew state then
                ( initialDifficulty rating, initialStability rating )

            else
                let
                    elapsed =
                        toFloat (elapsedDays state.lastReview now)

                    r =
                        retrievability elapsed state.stability

                    stability =
                        if elapsed <= 0 then
                            shortTermStability state.stability rating

                        else if rating == Again then
                            forgetStability state.difficulty state.stability r

                        else
                            recallStability state.difficulty state.stability r rating
                in
                ( nextDifficulty state.difficulty rating, clamp 0.001 36500 stability )
    in
    { due = addDays (nextIntervalDays desiredRetention newStability) now
    , stability = newStability
    , difficulty = newDifficulty
    , lastReview = now
    }
