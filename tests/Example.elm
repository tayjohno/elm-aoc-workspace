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
            [ test_answer "partOne" "3477353" Day01.partOne
            , test_answer "partTwo" "5213146" Day01.partTwo
            ]
        , describe "Day02"
            [ test_answer "partOne" "6730673" Day02.partOne
            , test_answer "partTwo" "3749" Day02.partTwo
            ]
        , describe "Day03"
            [ test_answer "partOne" "721" Day03.partOne
            , test_answer "partTwo" "7388" Day03.partTwo
            ]
        , describe "Day04"
            [ test_answer "partOne" "1099" Day04.partOne
            , test_answer "partTwo" "710" Day04.partTwo
            ]
        , describe "Day05"
            [ test_answer "partOne" "7566643" Day05.partOne
            , test_answer "partTwo" "9265694" Day05.partTwo
            ]
        , describe "Day06"
            [ test_answer "partOne" "160040" Day06.partOne
            , test_answer "partTwo" "373" Day06.partTwo
            ]
        , describe "Day07"
            [ test_answer "partOne" "567045" Day07.partOne
            , test_answer "partTwo" "39016654" Day07.partTwo
            ]
        , describe "Day08"
            [ test_answer "partOne" "1690" Day08.partOne
            , test_answer "partTwo" "ZPZUB" Day08.partTwo
            ]
        , describe "Day09"
            [ test_answer "partOne" "4080871669" Day09.partOne
            , test_answer "partTwo" "75202" Day09.partTwo
            ]
        , describe "Day10"
            [ test_answer "partOne" "260" Day10.partOne
            , test_answer "partTwo" "608" Day10.partTwo
            ]
        , describe "Day11"
            [ test_answer "partOne" "2016" Day11.partOne
            , test_answer "partTwo" "RAPRCBPH" Day11.partTwo
            ]
        , describe "Day12"
            [ test_answer "partOne" "10664" Day12.partOne
            , test_answer "partTwo" "303459551979256" Day12.partTwo
            ]
        , describe "Day13"
            [ test_answer "partOne" "355" Day13.partOne
            , test_answer "partTwo" "18371" Day13.partTwo
            ]
        , describe "Day14"
            [ test_answer "partOne" "654909" Day14.partOne
            , test_answer "partTwo" "2876992" Day14.partTwo
            ]
        , describe "Day15"
            [ test_answer "partOne" "270" Day15.partOne
            , test_answer "partTwo" "364" Day15.partTwo
            ]
        , describe "Day16"
            [ test_answer "partOne" "34841690" Day16.partOne
            , test_answer "partTwo" "48776785" Day16.partTwo
            ]
        ]
