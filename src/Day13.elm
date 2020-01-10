module Day13 exposing (partOne, partTwo)

{--- Day 13: Care Package ---

As you ponder the solitude of space and the ever-increasing three-hour
roundtrip for messages between you and Earth, you notice that the Space
Mail Indicator Light is blinking. To help keep you sane, the Elves have
sent you a care package.

It's a new game for the ship's arcade cabinet! Unfortunately, the arcade
is all the way on the other end of the ship. Surely, it won't be hard to
build your own - the care package even comes with schematics.

The arcade cabinet runs Intcode software like the game the Elves sent
(your puzzle input). It has a primitive screen capable of drawing square
tiles on a grid. The software draws tiles to the screen with output
instructions: every three output instructions specify the x position
(distance from the left), y position (distance from the top), and tile id.
The tile id is interpreted as follows:

  0 is an empty tile. No game object appears in this tile.
  1 is a wall tile. Walls are indestructible barriers.
  2 is a block tile. Blocks can be broken by the ball.
  3 is a horizontal paddle tile. The paddle is indestructible.
  4 is a ball tile. The ball moves diagonally and bounces off objects.

For example, a sequence of output values like 1,2,3,6,5,4 would draw a
horizontal paddle tile (1 tile from the left and 2 tiles from the top) and
a ball tile (6 tiles from the left and 5 tiles from the top).

Start the game. How many block tiles are on the screen when the game
exits?

--- Part Two ---

The game didn't run because you didn't put in any quarters. Unfortunately,
you did not bring any quarters. Memory address 0 represents the number of
quarters that have been inserted; set it to 2 to play for free.

The arcade cabinet has a joystick that can move left and right. The
software reads the position of the joystick with input instructions:

  If the joystick is in the neutral position, provide 0.
  If the joystick is tilted to the left, provide -1.
  If the joystick is tilted to the right, provide 1.

The arcade cabinet also has a segment display capable of showing a single
number that represents the player's current score. When three output
instructions specify X=-1, Y=0, the third output instruction is not a
tile; the value instead specifies the new score to show in the segment
display. For example, a sequence of output values like -1,0,12345 would
show 12345 as the player's current score.

Beat the game by breaking all the blocks. What is your score after the
last block is broken?


--}

import Answer exposing (Answer(..))
import Day13.Input exposing (input)
import Intcode
import Intcode.Memory
import List.Extra as List
import Matrix exposing (Matrix)


type Tile
    = Empty
    | Wall
    | Block
    | Paddle
    | Ball


type Joystick
    = Left
    | Right
    | Center


type alias GameState =
    { computer : Intcode.State
    , score : Int
    , screen : Matrix Tile
    }


partOne : () -> Answer String
partOne _ =
    input
        |> Intcode.executeProgram
        |> .outputs
        |> List.greedyGroupsOf 3
        |> List.foldl
            (\instruction blockList ->
                case instruction of
                    x :: y :: 2 :: [] ->
                        ( x, y ) :: blockList

                    _ :: _ :: _ :: [] ->
                        blockList

                    _ ->
                        Debug.todo "Expected sets of 3"
            )
            []
        |> List.unique
        |> List.length
        |> Answer.fromInt


partTwo : () -> Answer String
partTwo _ =
    playGame newGame
        |> .score
        |> Answer.fromInt


playGame : GameState -> GameState
playGame { computer, screen, score } =
    let
        newComputer =
            { computer | outputs = [] }

        newScreen =
            screen |> draw computer.outputs

        newScore =
            computer.outputs |> outputsToScore |> Maybe.withDefault score

        -- _ =
        -- newScreen |> printScreen
        -- _ =
        -- newScore |> Debug.log "Score"
        getXPosition tile =
            case newScreen |> Matrix.findAll ((==) tile) of
                [ ( _, ( x, _ ) ) ] ->
                    x

                _ ->
                    Debug.todo "not found"

        paddleX =
            getXPosition Paddle

        ballX =
            getXPosition Ball

        joystick =
            case compare ballX paddleX of
                LT ->
                    Left

                EQ ->
                    Center

                GT ->
                    Right
    in
    case computer.status of
        Intcode.Halted ->
            { computer = newComputer
            , screen = newScreen
            , score = newScore
            }

        _ ->
            playGame
                { computer =
                    newComputer
                        |> takeTurn joystick
                        |> Intcode.executeProgram
                , screen = newScreen
                , score = newScore
                }


takeTurn : Joystick -> Intcode.State -> Intcode.State
takeTurn joystick state =
    state
        |> Intcode.setInputs [ joystick |> toInput ]
        |> Intcode.executeProgram


insertTwoQuarters : Intcode.State -> Intcode.State
insertTwoQuarters state =
    { state
        | memory =
            state.memory
                |> Intcode.Memory.write 0 2
    }


newGame : GameState
newGame =
    let
        startingComputer =
            input |> insertTwoQuarters |> Intcode.executeProgram
    in
    { computer = { startingComputer | outputs = [] }
    , score = startingComputer.outputs |> outputsToScore |> Maybe.withDefault 0
    , screen = draw startingComputer.outputs (Matrix.empty ( 42, 24 ) Empty)
    }


toInput : Joystick -> Int
toInput joystick =
    case joystick of
        Left ->
            -1

        Right ->
            1

        Center ->
            0


draw : List Int -> Matrix Tile -> Matrix Tile
draw intList tileMatrix =
    intList
        |> List.greedyGroupsOf 3
        |> List.foldl drawPixel tileMatrix


drawPixel : List Int -> Matrix Tile -> Matrix Tile
drawPixel pixel screen =
    case pixel of
        x :: y :: t :: [] ->
            if x >= 0 && y >= 0 then
                screen
                    |> Matrix.set ( x, y ) (t |> intToTile)

            else
                screen

        a ->
            Debug.log "unexpected" a
                |> Debug.todo "oops"


outputsToScore : List Int -> Maybe Int
outputsToScore outputs =
    outputs
        |> List.greedyGroupsOf 3
        |> List.filterMap
            (\group ->
                case group of
                    x :: y :: score :: [] ->
                        if ( x, y ) == ( -1, 0 ) then
                            Just score

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.last


printScreen : Matrix Tile -> Matrix Tile
printScreen screen =
    screen
        |> Matrix.customPrint
            (\tile ->
                case tile of
                    Empty ->
                        ' '

                    Wall ->
                        '▮'

                    Block ->
                        '#'

                    Paddle ->
                        '—'

                    Ball ->
                        'o'
            )


intToTile : Int -> Tile
intToTile i =
    case i of
        0 ->
            Empty

        1 ->
            Wall

        2 ->
            Block

        3 ->
            Paddle

        4 ->
            Ball

        _ ->
            Debug.todo ("Unrecognized Tile Code (" ++ (i |> String.fromInt) ++ ")")
