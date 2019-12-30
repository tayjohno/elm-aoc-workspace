module IntcodeTests exposing (suite)

import Array
import Expect
import Intcode
import String.Extra as String
import String.Interpolate as String
import Test exposing (Test, describe, test)


simpleExecutionTest : String -> List Int -> List Int -> Test
simpleExecutionTest text program expectation =
    let
        title =
            String.interpolate "{0}: {1} -> {2}"
                [ text
                , printMemory program
                , printMemory expectation
                ]
    in
    test title <|
        \_ ->
            let
                finalMemory =
                    Intcode.init program
                        |> Intcode.executeProgram
                        |> .memory
                        |> Array.toList
            in
            Expect.equal finalMemory expectation


inputOutputTest : String -> List Int -> Int -> Int -> Test
inputOutputTest text program input expectedOutput =
    let
        title =
            String.interpolate "{0}: f({1}) expects output {2}"
                [ text
                , String.fromInt input
                , String.fromInt expectedOutput
                ]
    in
    test title <|
        \_ ->
            let
                finalState =
                    Intcode.init program
                        |> Intcode.setInputs [ input ]
                        |> Intcode.executeProgram
            in
            Expect.equal finalState.outputs [ expectedOutput ]


printMemory : List Int -> String
printMemory intList =
    "[" ++ (intList |> List.map String.fromInt |> String.join ",") ++ "]"


suite : Test
suite =
    describe "Intcode"
        [ describe "Simple tests"
            [ simpleExecutionTest "week 1 short"
                [ 1, 0, 0, 3, 99 ]
                [ 1, 0, 0, 2, 99 ]
            , simpleExecutionTest "week 1 medium"
                [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
                [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
            , simpleExecutionTest "week 2 ImmediateMode"
                [ 1002, 4, 3, 4, 33 ]
                [ 1002, 4, 3, 4, 99 ]
            , simpleExecutionTest "week 2 negative numbers"
                [ 1101, 100, -1, 4, 0 ]
                [ 1101, 100, -1, 4, 99 ]
            ]
        , describe "inputOutput tests"
            [ describe "equals 8 (PositionMode)"
                (let
                    testHelper : Int -> Int -> Test
                    testHelper =
                        inputOutputTest "[ f(x): x==8 ]" [ 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 ]
                 in
                 [ testHelper 7 0
                 , testHelper 8 1
                 , testHelper 9 0
                 ]
                )
            , describe "equals 8 (ImmediateMode)"
                (let
                    testHelper : Int -> Int -> Test
                    testHelper =
                        inputOutputTest "[ f(x): x==8 ]" [ 3, 3, 1108, -1, 8, 3, 4, 3, 99 ]
                 in
                 [ testHelper 7 0
                 , testHelper 8 1
                 , testHelper 9 0
                 ]
                )
            , describe "less than 8 (PositionMode)"
                (let
                    testHelper : Int -> Int -> Test
                    testHelper =
                        inputOutputTest "[ f(x): x<8 ]" [ 3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8 ]
                 in
                 [ testHelper 7 1
                 , testHelper 8 0
                 , testHelper 9 0
                 ]
                )
            , describe "less than 8 (ImmediateMode)"
                (let
                    testHelper : Int -> Int -> Test
                    testHelper =
                        inputOutputTest "[ f(x): x<8 ]" [ 3, 3, 1107, -1, 8, 3, 4, 3, 99 ]
                 in
                 [ testHelper 7 1
                 , testHelper 8 0
                 , testHelper 9 0
                 ]
                )
            , describe "zero jump test (PositionMode)"
                (let
                    testHelper : Int -> Int -> Test
                    testHelper =
                        inputOutputTest "[ f(x): x!=0 ]" [ 3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9 ]
                 in
                 [ testHelper -1 1
                 , testHelper 0 0
                 , testHelper 1 1
                 ]
                )
            , describe "day 5 larger example"
                (let
                    testHelper : Int -> Int -> Test
                    testHelper =
                        inputOutputTest "" [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99 ]
                 in
                 {- The above example program uses an input instruction to
                    ask for a single number. The program will then output 999
                    if the input value is below 8, output 1000 if the input
                    value is equal to 8, or output 1001 if the input value is
                    greater than 8.
                 -}
                 [ testHelper 6 999
                 , testHelper 7 999
                 , testHelper 8 1000
                 , testHelper 9 1001
                 , testHelper 10 1001
                 ]
                )
            ]
        ]
