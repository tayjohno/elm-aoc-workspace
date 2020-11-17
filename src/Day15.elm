module Day15 exposing (..)

{--- Day 15: Oxygen System ---

Out here in deep space, many things can go wrong. Fortunately, many of
those things have indicator lights. Unfortunately, one of those lights is
lit: the oxygen system for part of the ship has failed!

According to the readouts, the oxygen system must have failed days ago
after a rupture in oxygen tank two; that section of the ship was
automatically sealed once oxygen levels went dangerously low. A single
remotely-operated repair droid is your only option for fixing the oxygen
system.

The Elves' care package included an Intcode program (your puzzle input)
that you can use to remotely control the repair droid. By running that
program, you can direct the repair droid to the oxygen system and fix the
problem.

The remote control program executes the following steps in a loop forever:

  Accept a movement command via an input instruction.
  Send the movement command to the repair droid.
  Wait for the repair droid to finish the movement operation.
  Report on the status of the repair droid via an output instruction.

Only four movement commands are understood: north (1), south (2), west
(3), and east (4). Any other command is invalid. The movements differ in
direction, but not in distance: in a long enough east-west hallway, a
series of commands like 4,4,4,4,3,3,3,3 would leave the repair droid back
where it started.

The repair droid can reply with any of the following status codes:

  0: The repair droid hit a wall. Its position has not changed.
  1: The repair droid has moved one step in the requested direction.
  2: The repair droid has moved one step in the requested direction; its
     new position is the location of the oxygen system.

You don't know anything about the area around the repair droid, but you
can figure it out by watching the status codes.

For example, we can draw the area using D for the droid, # for walls, .
for locations the droid can traverse, and empty space for unexplored
locations. Then, the initial state looks like this:

  ````````
  `      `
  `      `
  `   D  `
  `      `
  `      `
  ````````

To make the droid go north, send it 1. If it replies with 0, you know that
location is a wall and that the droid didn't move:

  ````````
  `      `
  `   #  `
  `   D  `
  `      `
  `      `
  ````````

To move east, send 4; a reply of 1 means the movement was successful:

  ````````
  `      `
  `   #  `
  `   .D `
  `      `
  `      `
  ````````

Then, perhaps attempts to move north (1), south (2), and east (4) are all
met with replies of 0:

  ````````
  `      `
  `   ## `
  `   .D#`
  `    # `
  `      `
  ````````

Now, you know the repair droid is in a dead end. Backtrack with 3 (which
you already know will get a reply of 1 because you already know that
location is open):

  ````````
  `      `
  `   ## `
  `   D.#`
  `    # `
  `      `
  ````````

Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1,
south again (2) gets a reply of 0, and then west (3) gets a reply of 2:


  ````````
  `      `
  `   ## `
  `  #..#`
  `  D.# `
  `   #  `
  ````````

Now, because of the reply of 2, you know you've found the oxygen system!
In this example, it was only 2 moves away from the repair droid's starting
position.

What is the fewest number of movement commands required to move the repair
droid from its starting position to the location of the oxygen system?


--- Part Two ---

You quickly repair the oxygen system; oxygen gradually fills the area.

Oxygen starts in the location containing the repaired oxygen system. It
takes one minute for oxygen to spread to all open locations that are
adjacent to a location that already contains oxygen. Diagonal locations
are not adjacent.

In the example above, suppose you've used the droid to explore the area
fully and have the following map (where locations that currently contain
oxygen are marked O):

   ##
  #..##
  #.#..#
  #.O.#
   ###

Initially, the only location which contains oxygen is the location of the
repaired oxygen system. However, after one minute, the oxygen spreads to
all open (.) locations that are adjacent to a location containing oxygen:

   ##
  #..##
  #.#..#
  #OOO#
   ###

After a total of two minutes, the map looks like this:

   ##
  #..##
  #O#O.#
  #OOO#
   ###

After a total of three minutes:

   ##
  #O.##
  #O#OO#
  #OOO#
   ###

And finally, the whole region is full of oxygen after a total of four minutes:

   ##
  #OO##
  #O#OO#
  #OOO#
   ###

So, in this example, all locations contain oxygen after 4 minutes.

Use the repair droid to get a complete map of the area. How many minutes
will it take to fill with oxygen?


--}

import Answer exposing (Answer(..))
import Coordinate exposing (Coordinate, Direction(..))
import Day15.Input exposing (input)
import Intcode
import LazyMatrix exposing (LazyMatrix)
import Matrix


type SearchState
    = Found
    | Searching State


type alias State =
    { robots : List Robot
    , knownWorld : KnownWorld
    }


type alias Robot =
    { brains : Intcode.State
    , position : Coordinate
    }


type alias KnownWorld =
    LazyMatrix Tile


type Tile
    = Unknown
    | Wall
    | Hall
    | Oxygen


init : State
init =
    { robots = [ { brains = input, position = ( 0, 0 ) } ]
    , knownWorld = LazyMatrix.new Unknown |> LazyMatrix.set ( 0, 0 ) Hall
    }


{-| The basic implementation for part 1, keeps track of the search depth and the
locations of the robots searching as a depth-first search is performed.
-}
distanceToOxygen : Int -> State -> Int
distanceToOxygen depth state =
    case state |> searchDeeper of
        Found ->
            depth

        Searching updatedState ->
            distanceToOxygen (depth + 1) updatedState


{-| Takes in a state, extracts all robots, moves robots in every valid
direction (depth first search), returns the new state.
-}
searchDeeper : State -> SearchState
searchDeeper state =
    List.foldl searchAllDirectionsWithRobot (Searching { state | robots = [] }) state.robots


{-| given a single robot and the current state, send that robot in every
direction that hasn't been explored.
-}
searchAllDirectionsWithRobot : Robot -> SearchState -> SearchState
searchAllDirectionsWithRobot robot resultState =
    List.foldl (searchWithRobot robot) resultState [ Up, Left, Right, Down ]


{-| attempt to move a robot in a particular direction and see if the oxygen is
found.
-}
searchWithRobot : Robot -> Direction -> SearchState -> SearchState
searchWithRobot robot direction resultState =
    case resultState of
        Found ->
            Found

        Searching state ->
            let
                newCoordinate =
                    Coordinate.move direction robot.position

                isExplored =
                    state.knownWorld
                        |> LazyMatrix.get newCoordinate
                        |> (/=) Unknown
            in
            if isExplored then
                resultState

            else
                let
                    performedBrains =
                        robot.brains
                            |> Intcode.setInputs [ direction |> toInstruction ]
                            |> Intcode.executeProgram
                in
                case performedBrains.outputs of
                    [ 0 ] ->
                        Searching
                            { state
                                | knownWorld =
                                    state.knownWorld
                                        |> LazyMatrix.set newCoordinate Wall
                            }

                    [ 1 ] ->
                        Searching
                            { state
                                | knownWorld =
                                    state.knownWorld
                                        |> LazyMatrix.set newCoordinate Hall
                                , robots =
                                    { position = newCoordinate
                                    , brains = { performedBrains | outputs = [] }
                                    }
                                        :: state.robots
                            }

                    [ 2 ] ->
                        Found

                    a ->
                        Debug.log "unexpected" a
                            |> Debug.todo "oops"


{-| Looks everywhere until there's nothing left to be explored, then returns the map
-}
exploreEverywhere : State -> KnownWorld
exploreEverywhere state =
    -- let
    --     _ =
    --         state.knownWorld |> printKnownWorld
    -- in
    case state.robots of
        [] ->
            state.knownWorld

        _ ->
            List.foldl exploreAllDirectionsWithRobot { state | robots = [] } state.robots
                |> exploreEverywhere


exploreAllDirectionsWithRobot : Robot -> State -> State
exploreAllDirectionsWithRobot robot state =
    List.foldl (exploreWithRobot robot) state [ Up, Left, Right, Down ]


exploreWithRobot : Robot -> Direction -> State -> State
exploreWithRobot robot direction state =
    let
        newCoordinate =
            Coordinate.move direction robot.position

        isExplored =
            state.knownWorld
                |> LazyMatrix.get newCoordinate
                |> (/=) Unknown
    in
    if isExplored then
        -- Don't re-explore a location we've already been!
        state

    else
        let
            performedBrains =
                robot.brains
                    |> Intcode.setInputs [ direction |> toInstruction ]
                    |> Intcode.executeProgram
        in
        case performedBrains.outputs of
            [ 0 ] ->
                { state
                    | knownWorld =
                        state.knownWorld
                            |> LazyMatrix.set newCoordinate Wall
                }

            [ 1 ] ->
                { state
                    | knownWorld =
                        state.knownWorld
                            |> LazyMatrix.set newCoordinate Hall
                    , robots =
                        { position = newCoordinate
                        , brains = { performedBrains | outputs = [] }
                        }
                            :: state.robots
                }

            [ 2 ] ->
                { state
                    | knownWorld =
                        state.knownWorld
                            |> LazyMatrix.set newCoordinate Oxygen
                    , robots =
                        { position = newCoordinate
                        , brains = { performedBrains | outputs = [] }
                        }
                            :: state.robots
                }

            a ->
                Debug.log "unexpected" a
                    |> Debug.todo "oops"


type alias OxygenState =
    { recentExpansions : List Coordinate
    , knownWorld : KnownWorld
    , timeElapsed : Int
    }


oxygenCapacityReached : OxygenState -> Bool
oxygenCapacityReached oxygenState =
    oxygenState.knownWorld
        |> LazyMatrix.contains (\tile -> tile == Hall)
        |> not


timeToFullOxygen : OxygenState -> Int
timeToFullOxygen oxygenState =
    -- let
    --     _ =
    --         oxygenState.knownWorld |> printKnownWorld
    -- in
    if oxygenState |> oxygenCapacityReached then
        oxygenState.timeElapsed

    else
        let
            newMinute =
                { oxygenState
                    | recentExpansions = []
                    , timeElapsed = oxygenState.timeElapsed + 1
                }
        in
        List.foldl expandOxygenOutward newMinute oxygenState.recentExpansions
            |> timeToFullOxygen


expandOxygenOutward : Coordinate -> OxygenState -> OxygenState
expandOxygenOutward coordinate oxygenState =
    coordinate
        |> Coordinate.fourNeighbors
        |> List.foldl expandOxygenTo oxygenState


expandOxygenTo : Coordinate -> OxygenState -> OxygenState
expandOxygenTo coordinate oxygenState =
    case oxygenState.knownWorld |> LazyMatrix.get coordinate of
        Hall ->
            { oxygenState
                | recentExpansions = coordinate :: oxygenState.recentExpansions
                , knownWorld = oxygenState.knownWorld |> LazyMatrix.set coordinate Oxygen
            }

        _ ->
            oxygenState


toInstruction : Direction -> Int
toInstruction direction =
    case direction of
        Up ->
            1

        Down ->
            2

        Left ->
            3

        Right ->
            4


partOne : () -> Answer String
partOne _ =
    init
        |> distanceToOxygen 1
        |> Answer.fromInt


partTwo : () -> Answer String
partTwo _ =
    let
        knownWorld =
            init
                |> exploreEverywhere

        oxygenStart =
            case knownWorld |> LazyMatrix.find (\tile -> tile == Oxygen) of
                Nothing ->
                    Debug.todo "No oxygen in the world?"

                Just coord ->
                    coord |> List.singleton
    in
    timeToFullOxygen (OxygenState oxygenStart knownWorld 0)
        |> Answer.fromInt



-- DEBUGGING/HELPERS


printKnownWorld : KnownWorld -> KnownWorld
printKnownWorld knownWorld =
    let
        _ =
            Debug.log "" ""

        _ =
            Debug.log "" ""
    in
    knownWorld
        |> LazyMatrix.toMatrix
        |> Matrix.customPrint
            (\tile ->
                case tile of
                    Unknown ->
                        ' '

                    Wall ->
                        '#'

                    Hall ->
                        '.'

                    Oxygen ->
                        'O'
            )
        |> always knownWorld
