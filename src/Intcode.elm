module Intcode exposing (State, Status(..), addInput, executeProgram, init, setInputs)

import Array exposing (Array)
import List.Extra as List
import Maybe.Extra as Maybe


type alias Memory =
    Array Int


type alias State =
    { memory : Array Int
    , pointer : Pointer
    , status : Status
    , inputs : List Int
    , outputs : List Int
    , offset : Int
    }


type alias Pointer =
    Int


type alias Instruction =
    State -> Result String State


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
    { memory = program |> Array.fromList
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
        Err "halt" ->
            { state | status = Halted }

        Err "await" ->
            { state | status = AwaitingInput }

        Err a ->
            Debug.todo ("Unexpected error " ++ a)

        Ok newState ->
            executeProgram newState


executeInstruction : State -> Result String State
executeInstruction state =
    case getInstruction state of
        Ok instruction ->
            instruction state

        Err a ->
            Err a


getInstruction : State -> Result String Instruction
getInstruction { memory, pointer } =
    case Array.get pointer memory of
        Nothing ->
            errorWithInt "Invalid Op Code: Memory out of bounds" pointer

        Just a ->
            case remainderBy 100 a of
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


errorOutOfBounds pointer =
    errorWithInt "Memory out of bounds" pointer


errorWithInt : String -> Int -> Result String a
errorWithInt msg pointer =
    errorStringWithInt msg pointer |> Err


errorStringWithInt : String -> Int -> String
errorStringWithInt msg pointer =
    msg
        ++ " ("
        ++ (pointer |> String.fromInt)
        ++ ")"


readParam : ParameterMode -> Memory -> Pointer -> Result String Int
readParam readMode memory pointer =
    let
        rawParamValue =
            Array.get pointer memory
                |> Result.fromMaybe (errorStringWithInt "Parameter is out of bounds" pointer)
    in
    case readMode of
        ImmediateMode ->
            rawParamValue

        PositionMode ->
            rawParamValue
                |> Result.map
                    (\p -> Array.get p memory |> Maybe.withDefault 0)

        RelativeMode ->
            Debug.todo "oops"


{-| Read out a list of parameter types from the current memory position
-}
readParamTypes : Memory -> Pointer -> List ParameterMode
readParamTypes memory pointer =
    case memory |> Array.get pointer of
        Nothing ->
            Debug.todo <| "Trying to readParamTypes at invalid memory address " ++ String.fromInt pointer

        Just record ->
            record
                |> String.fromInt
                |> String.toList
                |> List.reverse
                |> List.drop 2
                |> List.map
                    (\i ->
                        case i of
                            '0' ->
                                PositionMode

                            '1' ->
                                ImmediateMode

                            '2' ->
                                RelativeMode

                            _ ->
                                Debug.todo <| "Invalid mode " ++ String.fromChar i
                    )


getParamType : Int -> List ParameterMode -> ParameterMode
getParamType i =
    List.getAt i >> Maybe.withDefault PositionMode



------ OPERATIONS ------


{-| opcode: 1
-}
addOp : Instruction
addOp state =
    let
        { memory, pointer } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)

        yResult =
            readParam (getParamType 1 paramTypes) memory (pointer + 2)

        outputPointerResult =
            Array.get (pointer + 3) memory
                |> Result.fromMaybe (errorStringWithInt "Parameter out of bounds" (pointer + 3))
    in
    case ( xResult, yResult, outputPointerResult ) of
        ( Err a, _, _ ) ->
            Err a

        ( _, Err a, _ ) ->
            Err a

        ( _, _, Err a ) ->
            Err a

        ( Ok x, Ok y, Ok outputPointer ) ->
            if outputPointer >= Array.length memory || outputPointer < 0 then
                errorWithInt "Invalid output pointer: Can't write to address" outputPointer

            else
                Ok
                    { state
                        | memory = Array.set outputPointer (x + y) memory
                        , pointer = pointer + 4
                    }


{-| opcode: 2
-}
multOp : Instruction
multOp state =
    let
        { memory, pointer } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)

        yResult =
            readParam (getParamType 1 paramTypes) memory (pointer + 2)

        outputPointerResult =
            Array.get (pointer + 3) memory
                |> Result.fromMaybe (errorStringWithInt "Parameter out of bounds" (pointer + 3))
    in
    case ( xResult, yResult, outputPointerResult ) of
        ( Err a, _, _ ) ->
            Err a

        ( _, Err a, _ ) ->
            Err a

        ( _, _, Err a ) ->
            Err a

        ( Ok x, Ok y, Ok outputPointer ) ->
            if outputPointer >= Array.length memory || outputPointer < 0 then
                errorWithInt "Invalid output pointer: Can't write to address" outputPointer

            else
                Ok
                    { state
                        | memory = Array.set outputPointer (x * y) memory
                        , pointer = pointer + 4
                    }


{-| opcode: 3

takes a single integer as input and saves it to the position given by its
only parameter. For example, the instruction 3,50 would take an input
value and store it at address 50.

-}
inputOp : Instruction
inputOp state =
    let
        { memory, pointer, inputs } =
            state

        outputPointerResult =
            Array.get (pointer + 1) memory
                |> Result.fromMaybe (errorStringWithInt "Parameter out of bounds" (pointer + 1))

        maybeInput =
            List.head inputs
    in
    outputPointerResult
        |> Result.andThen
            (\outputPointer ->
                if outputPointer >= Array.length memory || outputPointer < 0 then
                    errorWithInt "Invalid output pointer: Can't write to address" outputPointer

                else
                    case maybeInput of
                        Nothing ->
                            Err "await"

                        Just input ->
                            Ok
                                { state
                                    | memory = Array.set outputPointer input memory
                                    , inputs = inputs |> List.drop 1
                                    , pointer = pointer + 2
                                }
            )


{-| opcode: 4

outputs the value of its only parameter. For example, the instruction 4,50
would output the value at address 50.

-}
outputOp : Instruction
outputOp state =
    let
        { memory, pointer, outputs } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)
    in
    case xResult of
        Err a ->
            Err a

        Ok x ->
            Ok
                { state
                    | pointer = pointer + 2
                    , outputs = outputs ++ [ x ]
                }


{-| opcode: 5

if the first parameter is non-zero, it sets the instruction pointer to the
value from the second parameter. Otherwise, it does nothing.

-}
jumpIfTrueOp : Instruction
jumpIfTrueOp state =
    let
        { memory, pointer } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)

        yResult =
            readParam (getParamType 1 paramTypes) memory (pointer + 2)
    in
    xResult
        |> Result.andThen
            (\x ->
                if x == 0 then
                    -- Don't jump
                    Ok { state | pointer = pointer + 3 }

                else
                    yResult
                        |> Result.andThen
                            (\y ->
                                -- Jump
                                Ok { state | pointer = y }
                            )
            )


{-| opcode: 6

if the first parameter is zero, it sets the instruction pointer to the
value from the second parameter. Otherwise, it does nothing.

-}
jumpIfFalseOp : Instruction
jumpIfFalseOp state =
    let
        { memory, pointer } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)

        yResult =
            readParam (getParamType 1 paramTypes) memory (pointer + 2)
    in
    xResult
        |> Result.andThen
            (\x ->
                if x == 0 then
                    yResult
                        |> Result.andThen
                            (\y ->
                                -- Jump
                                Ok { state | pointer = y }
                            )

                else
                    -- Don't jump
                    Ok { state | pointer = pointer + 3 }
            )


{-| opcode: 7

if the first parameter is less than the second parameter, it stores 1 in
the position given by the third parameter. Otherwise, it stores 0.

-}
lessThanOp : Instruction
lessThanOp state =
    let
        { memory, pointer } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)

        yResult =
            readParam (getParamType 1 paramTypes) memory (pointer + 2)

        outputPointerResult =
            Array.get (pointer + 3) memory
                |> Result.fromMaybe (errorStringWithInt "Parameter out of bounds" (pointer + 3))
    in
    case ( xResult, yResult, outputPointerResult ) of
        ( Err a, _, _ ) ->
            Err a

        ( _, Err a, _ ) ->
            Err a

        ( _, _, Err a ) ->
            Err a

        ( Ok x, Ok y, Ok outputPointer ) ->
            let
                output =
                    if x < y then
                        1

                    else
                        0
            in
            Ok
                { state
                    | memory = memory |> Array.set outputPointer output
                    , pointer = pointer + 4
                }


{-| opcode: 8

if the first parameter is equal to the second parameter, it stores 1 in
the position given by the third parameter. Otherwise, it stores 0.

-}
equalsOp : Instruction
equalsOp state =
    let
        { memory, pointer } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)

        yResult =
            readParam (getParamType 1 paramTypes) memory (pointer + 2)

        outputPointerResult =
            Array.get (pointer + 3) memory
                |> Result.fromMaybe (errorStringWithInt "Parameter out of bounds" (pointer + 3))
    in
    case ( xResult, yResult, outputPointerResult ) of
        ( Err a, _, _ ) ->
            Err a

        ( _, Err a, _ ) ->
            Err a

        ( _, _, Err a ) ->
            Err a

        ( Ok x, Ok y, Ok outputPointer ) ->
            let
                output =
                    if x == y then
                        1

                    else
                        0
            in
            Ok
                { state
                    | memory = memory |> Array.set outputPointer output
                    , pointer = pointer + 4
                }


{-| opcode: 9

adjusts the relative base by the value of its only parameter. The relative
base increases (or decreases, if the value is negative) by the value of
the parameter.

-}
adjustRelativeBaseOp : Instruction
adjustRelativeBaseOp state =
    let
        { memory, pointer, offset } =
            state

        paramTypes =
            readParamTypes memory pointer

        xResult =
            readParam (getParamType 0 paramTypes) memory (pointer + 1)
    in
    xResult
        |> Result.andThen
            (\x ->
                Ok
                    { state
                        | pointer = pointer + 2
                        , offset = offset + x
                    }
            )


{-| opcode: 99
-}
haltOp : Instruction
haltOp _ =
    Err "halt"
