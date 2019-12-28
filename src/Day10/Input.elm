module Day10.Input exposing (..)

import Matrix exposing (Matrix)
import MatrixParser


type Tile
    = Empty
    | Asteroid


input : Matrix Tile
input =
    rawInput
        |> MatrixParser.parse Empty charToTile


charToTile : Char -> Tile
charToTile char =
    case char of
        '#' ->
            Asteroid

        _ ->
            Empty


tileToChar : Tile -> Char
tileToChar tile =
    case tile of
        Empty ->
            ' '

        Asteroid ->
            '#'


rawInput =
    """
##.##..#.####...#.#.####
##.###..##.#######..##..
..######.###.#.##.######
.#######.####.##.#.###.#
..#...##.#.....#####..##
#..###.#...#..###.#..#..
###..#.##.####.#..##..##
.##.##....###.#..#....#.
########..#####..#######
##..#..##.#..##.#.#.#..#
##.#.##.######.#####....
###.##...#.##...#.######
###...##.####..##..#####
##.#...#.#.....######.##
.#...####..####.##...##.
#.#########..###..#.####
#.##..###.#.######.#####
##..##.##...####.#...##.
###...###.##.####.#.##..
####.#.....###..#.####.#
##.####..##.#.##..##.#.#
#####..#...####..##..#.#
.##.##.##...###.##...###
..###.########.#.###..#.
    """
        |> String.trim
