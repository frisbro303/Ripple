module Ops.OpsLogTest exposing (suite)

import Expect
import Ops.Op exposing (Op, OpId(..), OpKind(..))
import Ops.OpsLog as OpsLog
import Random
import Set
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


suite : Test
suite =
    describe "Ops.OpsLog"
        [ test "inserting the same op twice doesn't duplicate it" <|
            \_ ->
                let
                    theOp =
                        op 1 1000 (SetRetention 90)

                    log =
                        OpsLog.emptyOpsLog |> OpsLog.insert theOp |> OpsLog.insert theOp
                in
                OpsLog.toList log |> List.length |> Expect.equal 1
        , test "two ops with the same timestamp but different ids are both kept" <|
            \_ ->
                let
                    log =
                        OpsLog.fromList
                            [ op 1 1000 (SetRetention 90)
                            , op 2 1000 (SetRetention 80)
                            ]
                in
                OpsLog.toList log |> List.length |> Expect.equal 2
        , test "merge is a union that keeps both logs' ops" <|
            \_ ->
                let
                    a =
                        OpsLog.fromList [ op 1 1000 (SetRetention 90) ]

                    b =
                        OpsLog.fromList [ op 2 2000 (SetRetention 80) ]

                    merged =
                        OpsLog.merge a b
                in
                OpsLog.toList merged |> List.length |> Expect.equal 2
        , test "merge is idempotent for overlapping ops" <|
            \_ ->
                let
                    shared =
                        op 1 1000 (SetRetention 90)

                    a =
                        OpsLog.fromList [ shared, op 2 2000 (SetRetention 80) ]

                    b =
                        OpsLog.fromList [ shared, op 3 3000 (SetRetention 70) ]

                    merged =
                        OpsLog.merge a b
                in
                OpsLog.toList merged |> List.length |> Expect.equal 3
        , test "diff returns only the ops present in the first log but not the second" <|
            \_ ->
                let
                    shared =
                        op 1 1000 (SetRetention 90)

                    onlyInA =
                        op 2 2000 (SetRetention 80)

                    a =
                        OpsLog.fromList [ shared, onlyInA ]

                    b =
                        OpsLog.fromList [ shared ]
                in
                OpsLog.diff a b |> OpsLog.toList |> Expect.equal [ onlyInA ]
        , test "idStrings reflects every op's id, deduplicated by uuid string" <|
            \_ ->
                let
                    log =
                        OpsLog.fromList
                            [ op 1 1000 (SetRetention 90)
                            , op 1 2000 (SetRetention 80)
                            ]
                in
                OpsLog.idStrings log |> Set.size |> Expect.equal 1
        , test "foldl visits every op in the log" <|
            \_ ->
                let
                    log =
                        OpsLog.fromList
                            [ op 1 1000 (SetRetention 90)
                            , op 2 2000 (SetRetention 80)
                            , op 3 3000 (SetRetention 70)
                            ]
                in
                OpsLog.foldl (\_ acc -> acc + 1) 0 log |> Expect.equal 3
        ]
