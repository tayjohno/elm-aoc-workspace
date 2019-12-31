module Intcode exposing (State, Status(..), addInput, executeInstruction, executeProgram, init, setInputs)

import Array exposing (Array)
import Array.Extra as Array
import Intcode.Int as Int
import Intcode.Memory as Memory exposing (Memory)
import List.Extra as List
import Maybe.Extra as Maybe


type alias State =
    { memory : Memory
    , pointer : Pointer
    , status : Status
    , inputs : List Int
    , outputs : List Int
    , offset : Int
    }


type alias Pointer =
    Int


type alias Instruction =
    State -> Result ErrType State


type ErrType
    = Halt
    | Await
    | Unknown String


type ParameterMode
    = PositionMode
    | ImmediateMode
    | RelativeMode


type Status
    = Ready
    | Halted
    | AwaitingInput


init : List Int -> State
init program =
    { memory = Memory.fromList program
    , status = Ready
    , inputs = []
    , pointer = 0
    , outputs = []
    , offset = 0
    }


setInputs : List Int -> State -> State
setInputs inputs state =
    { state | inputs = inputs, status = Ready }


addInput : Int -> State -> State
addInput newInput state =
    { state | inputs = state.inputs ++ [ newInput ], status = Ready }


executeProgram : State -> State
executeProgram state =
    case executeInstruction state of
        Err Halt ->
            { state | status = Halted }

        Err Await ->
            { state | status = AwaitingInput }

        Err (Unknown a) ->
            Debug.todo ("Unexpected error " ++ a)

        Ok newState ->
            executeProgram newState


executeInstruction : State -> Result ErrType State
executeInstruction state =
    case getInstruction state of
        Ok instruction ->
            instruction state

        Err a ->
            Err a


readVariableAndIncrement : State -> ( Int, State )
readVariableAndIncrement state =
    ( Memory.read state.pointer state.memory
    , { state | pointer = state.pointer + 1 }
    )


getInstruction : State -> Result ErrType Instruction
getInstruction { memory, pointer } =
    case Memory.read pointer memory |> remainderBy 100 of
        1 ->
            Ok addOp

        2 ->
            Ok multOp

        3 ->
            Ok inputOp

        4 ->
            Ok outputOp

        5 ->
            Ok jumpIfTrueOp

        6 ->
            Ok jumpIfFalseOp

        7 ->
            Ok lessThanOp

        8 ->
            Ok equalsOp

        9 ->
            Ok adjustRelativeBaseOp

        99 ->
            Ok haltOp

        code ->
            errorWithInt "Invalid Op Code: There is no operation with this code" code


errorWithInt : String -> Int -> Result ErrType a
errorWithInt msg pointer =
    errorStringWithInt msg pointer |> Unknown |> Err


errorStringWithInt : String -> Int -> String
errorStringWithInt msg pointer =
    msg
        ++ " ("
        ++ (pointer |> String.fromInt)
        ++ ")"


readParam : ParameterMode -> State -> ( Int, State )
readParam paramMode state0 =
    let
        ( rawParamValue, state1 ) =
            readVariableAndIncrement state0
    in
    case paramMode of
        ImmediateMode ->
            ( rawParamValue, state1 )

        PositionMode ->
            ( state1.memory |> Memory.read rawParamValue
            , state1
            )

        RelativeMode ->
            ( state1.memory |> Memory.read (rawParamValue + state1.offset)
            , state1
            )


getOutputPointer : ParameterMode -> State -> ( Int, State )
getOutputPointer paramMode state0 =
    let
        ( rawParamValue, state1 ) =
            readVariableAndIncrement state0
    in
    case paramMode of
        ImmediateMode ->
            Debug.todo "cannout output in ImmediateMode"

        PositionMode ->
            ( rawParamValue
            , state1
            )

        RelativeMode ->
            ( rawParamValue + state1.offset
            , state1
            )


{-| Read out a list of parameter types from the current memory position
-}
readParamTypes : State -> ( List ParameterMode, State )
readParamTypes state =
    let
        incrementedState =
            { state | pointer = state.pointer + 1 }

        record =
            state.memory
                |> Memory.read state.pointer

        charToType c =
            case c of
                '0' ->
                    PositionMode

                '1' ->
                    ImmediateMode

                '2' ->
                    RelativeMode

                _ ->
                    Debug.todo <| "Invalid mode " ++ String.fromChar c
    in
    ( record
        |> String.fromInt
        |> String.toList
        |> List.reverse
        |> List.drop 2
        |> List.map charToType
    , incrementedState
    )


getParamType : Int -> List ParameterMode -> ParameterMode
getParamType i =
    List.getAt i >> Maybe.withDefault PositionMode



------ OPERATIONS ------


{-| opcode: 1
-}
addOp : Instruction
addOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1

        ( y, state3 ) =
            readParam (getParamType 1 paramTypes) state2

        ( outputPointer, state4 ) =
            getOutputPointer (getParamType 2 paramTypes) state3
    in
    Ok
        { state4
            | memory =
                Memory.write outputPointer (x + y) state4.memory
        }


{-| opcode: 2
-}
multOp : Instruction
multOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1

        ( y, state3 ) =
            readParam (getParamType 1 paramTypes) state2

        ( outputPointer, state4 ) =
            getOutputPointer (getParamType 2 paramTypes) state3
    in
    Ok
        { state4
            | memory =
                Memory.write outputPointer (x * y) state4.memory
        }


{-| opcode: 3

takes a single integer as input and saves it to the position given by its
only parameter. For example, the instruction 3,50 would take an input
value and store it at address 50.

-}
inputOp : Instruction
inputOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( outputPointer, state2 ) =
            getOutputPointer (getParamType 0 paramTypes) state1

        maybeInput =
            List.head state2.inputs
    in
    case maybeInput of
        Nothing ->
            Err Await

        Just input ->
            Ok
                { state2
                    | memory = state2.memory |> Memory.write outputPointer input
                    , inputs = state2.inputs |> List.drop 1
                }


{-| opcode: 4

outputs the value of its only parameter. For example, the instruction `4,50`
would output the value at address `50`.

-}
outputOp : Instruction
outputOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1
    in
    Ok
        { state2
            | outputs = state2.outputs ++ [ x ]
        }


{-| opcode: 5

if the first parameter is non-zero, it sets the instruction pointer to the
value from the second parameter. Otherwise, it does nothing.

-}
jumpIfTrueOp : Instruction
jumpIfTrueOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1

        ( y, state3 ) =
            readParam (getParamType 1 paramTypes) state2
    in
    Ok
        (if x == 0 then
            -- Don't jump
            state3

         else
            -- Jump
            { state3 | pointer = y }
        )


{-| opcode: 6

if the first parameter is zero, it sets the instruction pointer to the
value from the second parameter. Otherwise, it does nothing.

-}
jumpIfFalseOp : Instruction
jumpIfFalseOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1

        ( y, state3 ) =
            readParam (getParamType 1 paramTypes) state2
    in
    Ok
        (if x == 0 then
            { state3 | pointer = y }

         else
            -- Don't jump
            state3
        )


{-| opcode: 7

if the first parameter is less than the second parameter, it stores 1 in
the position given by the third parameter. Otherwise, it stores 0.

-}
lessThanOp : Instruction
lessThanOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1

        ( y, state3 ) =
            readParam (getParamType 1 paramTypes) state2

        ( outputPointer, state4 ) =
            getOutputPointer (getParamType 2 paramTypes) state3

        output =
            (x < y) |> Int.fromBool
    in
    Ok
        { state4
            | memory =
                state4.memory
                    |> Memory.write outputPointer output
        }


{-| opcode: 8

if the first parameter is equal to the second parameter, it stores 1 in
the position given by the third parameter. Otherwise, it stores 0.

-}
equalsOp : Instruction
equalsOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1

        ( y, state3 ) =
            readParam (getParamType 1 paramTypes) state2

        ( outputPointer, state4 ) =
            getOutputPointer (getParamType 2 paramTypes) state3

        output =
            (x == y) |> Int.fromBool
    in
    Ok
        { state4
            | memory =
                state4.memory
                    |> Memory.write outputPointer output
        }


{-| opcode: 9

adjusts the relative base by the value of its only parameter. The relative
base increases (or decreases, if the value is negative) by the value of
the parameter.

-}
adjustRelativeBaseOp : Instruction
adjustRelativeBaseOp state0 =
    let
        ( paramTypes, state1 ) =
            readParamTypes state0

        ( x, state2 ) =
            readParam (getParamType 0 paramTypes) state1
    in
    Ok
        { state2
            | offset = state2.offset + x
        }


{-| opcode: 99
-}
haltOp : Instruction
haltOp _ =
    Err Halt
