module Sea.Sea exposing (Sea, fromOpsLog, introducedCardIds, isIntroduced, newCardsToday, nextDue, toList)

import Dict exposing (Dict)
import Ops.Op exposing (Op, OpKind(..))
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sea.Card as Card
import Sea.FSRS as FSRS
import Set
import Time exposing (Posix)
import UUID


type Sea
    = Sea { cards : Dict String Card.Card }


emptySea : Sea
emptySea =
    Sea { cards = Dict.empty }


toList : Sea -> List Card.Card
toList (Sea { cards }) =
    Dict.values cards


getDue : Posix -> Sea -> List Card.Card
getDue now (Sea { cards }) =
    cards
        |> Dict.values
        |> List.filter (Card.isDue now)


nextDue : Int -> Posix -> OpsLog -> Sea -> Maybe Card.Card
nextDue dailyNewLimit now opsLog sea =
    let
        allowNewIntroductions =
            newCardsToday now opsLog < dailyNewLimit

        introduced =
            introducedCardIds opsLog
    in
    getDue now sea
        |> List.filter (\card -> allowNewIntroductions || isIntroduced introduced card)
        |> List.sortBy (\card -> Time.posixToMillis card.fsrs.due)
        |> List.head


introducedCardIds : OpsLog -> Set.Set String
introducedCardIds opsLog =
    OpsLog.foldl
        (\op acc ->
            case op.opKind of
                ReviewCard { id } ->
                    Set.insert (UUID.toString id) acc

                _ ->
                    acc
        )
        Set.empty
        opsLog


isIntroduced : Set.Set String -> Card.Card -> Bool
isIntroduced introduced card =
    Set.member (UUID.toString card.id) introduced


newCardsToday : Posix -> OpsLog -> Int
newCardsToday now opsLog =
    let
        today =
            FSRS.dateOf now

        firstReviewByCard =
            OpsLog.foldl
                (\op acc ->
                    case op.opKind of
                        ReviewCard { id } ->
                            Dict.update (UUID.toString id)
                                (\existing ->
                                    case existing of
                                        Just t ->
                                            if Time.posixToMillis op.timeStamp < Time.posixToMillis t then
                                                Just op.timeStamp

                                            else
                                                Just t

                                        Nothing ->
                                            Just op.timeStamp
                                )
                                acc

                        _ ->
                            acc
                )
                Dict.empty
                opsLog
    in
    firstReviewByCard
        |> Dict.values
        |> List.filter (\t -> FSRS.dateOf t == today)
        |> List.length


getCard : Card.CardId -> Sea -> Maybe Card.Card
getCard id (Sea { cards }) =
    Dict.get (UUID.toString id) cards


insertCard : Card.Card -> Sea -> Sea
insertCard card (Sea sea) =
    Sea { sea | cards = Dict.insert (UUID.toString card.id) card sea.cards }


removeCard : Card.CardId -> Sea -> Sea
removeCard id (Sea sea) =
    Sea { sea | cards = Dict.remove (UUID.toString id) sea.cards }


updateCard : Card.CardId -> (Card.Card -> Card.Card) -> Sea -> Sea
updateCard id transform sea =
    case getCard id sea of
        Nothing ->
            sea

        Just card ->
            insertCard (transform card) sea


applyOp : Float -> Op -> Sea -> Sea
applyOp desiredRetention op sea =
    case op.opKind of
        CreateCard { id, front, back } ->
            insertCard
                (Card.new id front back op.timeStamp)
                sea

        EditCard { id, front, back } ->
            updateCard id
                (\card -> { card | front = front, back = back })
                sea

        DeleteCard id ->
            removeCard id sea

        ReviewCard { id, rating } ->
            updateCard id
                (Card.review desiredRetention op.timeStamp rating)
                sea

        ResetToLearning id ->
            updateCard id
                (Card.resetToLearning op.timeStamp)
                sea

        DeferCard { id, days } ->
            updateCard id
                (Card.defer days op.timeStamp)
                sea

        SetPreamble _ ->
            sea

        SetRetention _ ->
            sea

        AddImage _ ->
            sea


fromOpsLog : Float -> OpsLog -> Sea
fromOpsLog desiredRetention opsLog =
    OpsLog.foldl (applyOp desiredRetention) emptySea opsLog
