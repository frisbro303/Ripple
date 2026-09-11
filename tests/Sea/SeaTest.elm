module Sea.SeaTest exposing (suite)

import Expect
import Ops.Op exposing (Op, OpId(..), OpKind(..))
import Ops.OpsLog as OpsLog
import Random
import Sea.FSRS as FSRS exposing (Rating(..))
import Sea.Sea as Sea
import Test exposing (Test, describe, test)
import Time
import UUID exposing (UUID)


uuidFromSeed : Int -> UUID
uuidFromSeed seed =
    Random.step UUID.generator (Random.initialSeed seed) |> Tuple.first


op : Int -> Int -> OpKind -> Op
op seed millis kind =
    { id = OpId (uuidFromSeed seed)
    , timeStamp = Time.millisToPosix millis
    , opKind = kind
    }


now : Time.Posix
now =
    Time.millisToPosix 1000000


card1 : UUID
card1 =
    uuidFromSeed 101


card2 : UUID
card2 =
    uuidFromSeed 102


createCard : Int -> UUID -> Op
createCard millis id =
    op millis millis (CreateCard { id = id, front = "front", back = "back" })


suite : Test
suite =
    describe "Sea.Sea"
        [ test "a freshly created card is immediately due" <|
            \_ ->
                let
                    sea =
                        Sea.fromOpsLog 0.9 (OpsLog.fromList [ createCard 1 card1 ])
                in
                Sea.nextDue 20 now OpsLog.emptyOpsLog sea
                    |> Maybe.map .id
                    |> Expect.equal (Just card1)
        , test "a deleted card is never returned" <|
            \_ ->
                let
                    opsLog =
                        OpsLog.fromList
                            [ createCard 1 card1
                            , op 2 2 (DeleteCard card1)
                            ]

                    sea =
                        Sea.fromOpsLog 0.9 opsLog
                in
                Sea.nextDue 20 now opsLog sea |> Expect.equal Nothing
        , test "among several due cards, the earliest-due one is picked first" <|
            \_ ->
                let
                    opsLog =
                        OpsLog.fromList
                            [ createCard 1 card1
                            , createCard 2 card2
                            , op 3 3 (DeferCard { id = card1, days = 5 })
                            ]

                    sea =
                        Sea.fromOpsLog 0.9 opsLog
                in
                Sea.nextDue 20 now opsLog sea
                    |> Maybe.map .id
                    |> Expect.equal (Just card2)
        , test "once the daily new-card limit is hit, only already-introduced cards are offered" <|
            \_ ->
                let
                    opsLog =
                        OpsLog.fromList
                            [ createCard 1 card1
                            , createCard 2 card2
                            , op 3 3 (ReviewCard { id = card2, rating = Good })
                            ]

                    sea =
                        Sea.fromOpsLog 0.9 opsLog
                in
                Sea.nextDue 1 now opsLog sea
                    |> Maybe.map .id
                    |> Expect.equal (Just card2)
        , test "introducedCardIds only counts cards with at least one review" <|
            \_ ->
                let
                    opsLog =
                        OpsLog.fromList
                            [ createCard 1 card1
                            , createCard 2 card2
                            , op 3 3 (ReviewCard { id = card2, rating = Good })
                            ]

                    introduced =
                        Sea.introducedCardIds opsLog

                    asCard id =
                        { id = id, front = "", back = "", fsrs = FSRS.initialState now, learningStep = Nothing }
                in
                ( Sea.isIntroduced introduced (asCard card1), Sea.isIntroduced introduced (asCard card2) )
                    |> Expect.equal ( False, True )
        , test "newCardsToday counts each card's first review only once" <|
            \_ ->
                let
                    opsLog =
                        OpsLog.fromList
                            [ createCard 1 card1
                            , op 2 2 (ReviewCard { id = card1, rating = Again })
                            , op 3 3 (ReviewCard { id = card1, rating = Good })
                            ]
                in
                Sea.newCardsToday now opsLog |> Expect.equal 1
        ]
