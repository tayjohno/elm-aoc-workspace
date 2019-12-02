module Example exposing (suite)

import Answer exposing (Answer(..))
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


test_answer : String -> String -> (() -> Answer String) -> Test
test_answer part expectation method =
    test part <|
        \_ ->
            Expect.equal (Solved expectation) (method ())


suite : Test
suite =
    describe "Advent of Code"
        [ describe "Day01"
            [ test_answer "partOne" "" Day01.partOne
            , test_answer "partTwo" "" Day01.partTwo
            ]
        ]
