module Pages.Stats.Data exposing
    ( DayCounts
    , Forecast
    , ForecastDay
    , History
    , HistoryDay
    , RatingTotals
    , Summary
    , forecast
    , history
    , retentionTotals
    , summarize
    )

import Date exposing (Date)
import Dict exposing (Dict)
import Ops.Op exposing (OpKind(..))
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sea.FSRS as FSRS exposing (Rating(..))
import Sea.Sea as Sea exposing (Sea)
import Set
import Time


type alias Summary =
    { total : Int
    , due : Int
    , learning : Int
    , new : Int
    , dailyStreak : Int
    , retainedPercent : Maybe Int
    }


summarize : Int -> Time.Posix -> OpsLog -> Sea -> Summary
summarize dailyNewLimit now opsLog sea =
    let
        cards =
            Sea.toList sea

        ( notYetGraduated, reviewedCards ) =
            List.partition (\card -> FSRS.isNew card.fsrs) cards

        introduced =
            Sea.introducedCardIds opsLog

        ( learningCards, newCards ) =
            List.partition (Sea.isIntroduced introduced) notYetGraduated

        allowNewIntroductions =
            Sea.newCardsToday now opsLog < dailyNewLimit

        dueCards =
            cards
                |> List.filter (\card -> Time.posixToMillis card.fsrs.due <= Time.posixToMillis now)
                |> List.filter (\card -> allowNewIntroductions || Sea.isIntroduced introduced card)

        retainedPercent =
            case reviewedCards of
                [] ->
                    Nothing

                _ ->
                    let
                        retrievabilityOf card =
                            FSRS.retrievability
                                (toFloat (max 0 (FSRS.elapsedDays card.fsrs.lastReview now)))
                                card.fsrs.stability
                    in
                    Just
                        (round
                            (100
                                * (List.map retrievabilityOf reviewedCards |> List.sum)
                                / toFloat (List.length reviewedCards)
                            )
                        )
    in
    { total = List.length cards
    , due = List.length dueCards
    , learning = List.length learningCards
    , new = List.length newCards
    , dailyStreak = dailyStreak now opsLog
    , retainedPercent = retainedPercent
    }


dailyStreak : Time.Posix -> OpsLog -> Int
dailyStreak now opsLog =
    let
        reviewedDays =
            OpsLog.toList opsLog
                |> List.filterMap
                    (\op ->
                        case op.opKind of
                            ReviewCard _ ->
                                Just (dayNumber op.timeStamp)

                            _ ->
                                Nothing
                    )
                |> Set.fromList

        today =
            dayNumber now

        startDay =
            if Set.member today reviewedDays then
                today

            else
                today - 1

        countBack day =
            if Set.member day reviewedDays then
                1 + countBack (day - 1)

            else
                0
    in
    countBack startDay


dayNumber : Time.Posix -> Int
dayNumber t =
    Date.toRataDie (FSRS.dateOf t)


type alias DayCounts =
    { again : Int, hard : Int, good : Int, easy : Int }


emptyDayCounts : DayCounts
emptyDayCounts =
    { again = 0, hard = 0, good = 0, easy = 0 }


addRating : Rating -> DayCounts -> DayCounts
addRating rating counts =
    case rating of
        Again ->
            { counts | again = counts.again + 1 }

        Hard ->
            { counts | hard = counts.hard + 1 }

        Good ->
            { counts | good = counts.good + 1 }

        Easy ->
            { counts | easy = counts.easy + 1 }


type alias HistoryDay =
    { date : Date, counts : DayCounts }


type alias History =
    List HistoryDay


history : Time.Posix -> Int -> OpsLog -> History
history now days opsLog =
    let
        clampedDays =
            clamp 1 730 days

        today =
            FSRS.dateOf now

        byDay : Dict Int DayCounts
        byDay =
            OpsLog.toList opsLog
                |> List.filterMap
                    (\op ->
                        case op.opKind of
                            ReviewCard { rating } ->
                                Just ( Date.toRataDie (FSRS.dateOf op.timeStamp), rating )

                            _ ->
                                Nothing
                    )
                |> List.foldl
                    (\( day, rating ) acc ->
                        Dict.update day
                            (Maybe.withDefault emptyDayCounts >> addRating rating >> Just)
                            acc
                    )
                    Dict.empty
    in
    List.range 0 (clampedDays - 1)
        |> List.map
            (\offset ->
                let
                    day =
                        Date.add Date.Days (offset - (clampedDays - 1)) today
                in
                { date = day
                , counts = Dict.get (Date.toRataDie day) byDay |> Maybe.withDefault emptyDayCounts
                }
            )


type alias ForecastDay =
    { date : Date, count : Int }


type alias Forecast =
    List ForecastDay


forecast : Int -> Time.Posix -> OpsLog -> Sea -> Forecast
forecast days now opsLog sea =
    let
        clampedDays =
            clamp 1 730 days

        today =
            FSRS.dateOf now

        todayNum =
            Date.toRataDie today

        introduced =
            Sea.introducedCardIds opsLog

        byDay : Dict Int Int
        byDay =
            Sea.toList sea
                |> List.filter (Sea.isIntroduced introduced)
                |> List.map (\card -> max todayNum (Date.toRataDie (FSRS.dateOf card.fsrs.due)))
                |> List.foldl (\day acc -> Dict.update day (Maybe.withDefault 0 >> (+) 1 >> Just) acc) Dict.empty
    in
    List.range 0 (clampedDays - 1)
        |> List.map
            (\offset ->
                let
                    day =
                        Date.add Date.Days offset today
                in
                { date = day
                , count = Dict.get (Date.toRataDie day) byDay |> Maybe.withDefault 0
                }
            )


type alias RatingTotals =
    { again : Int, hard : Int, good : Int, easy : Int, total : Int }


retentionTotals : History -> RatingTotals
retentionTotals hist =
    let
        sumBy getter =
            List.sum (List.map (getter << .counts) hist)

        again =
            sumBy .again

        hard =
            sumBy .hard

        good =
            sumBy .good

        easy =
            sumBy .easy
    in
    { again = again, hard = hard, good = good, easy = easy, total = again + hard + good + easy }
