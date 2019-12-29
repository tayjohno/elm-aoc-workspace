module Intcode exposing (Memory, Pointer, State, executeProgram, init)

import Array exposing (Array)


type alias Memory =
    Array Int


type alias State =
    { memory : Array Int
    , pointer : Pointer
    }


type alias Pointer =
    Int


type alias Instruction =
    State -> Result String State


type ParameterMode
    = PositionMode
    | ImmediateMode


init : Memory -> State
init memory =
    { memory = memory
    , pointer = 0
    }


executeProgram : State -> State
executeProgram state =
    case executeInstruction state of
        Err "halt" ->
            state

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
        Just 1 ->
            Ok add

        Just 2 ->
            Ok mult

        Just 99 ->
            Ok halt

        Just a ->
            errorWithInt "Invalid Op Code: There is no operation with this code" a

        Nothing ->
            errorWithInt "Invalid Op Code: Memory out of bounds" pointer


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
    case readMode of
        ImmediateMode ->
            Array.get pointer memory
                |> Result.fromMaybe (errorStringWithInt "Parameter is out of bounds" pointer)

        PositionMode ->
            Array.get pointer memory
                |> Result.fromMaybe (errorStringWithInt "Parameter is out of bounds" pointer)
                |> Result.andThen
                    (\p ->
                        Array.get p memory
                            |> Result.fromMaybe (errorStringWithInt "PositionMode parameter points out of bounds" p)
                    )


get2Params : ( ParameterMode, ParameterMode ) -> Memory -> Pointer -> Result String ( Int, Int )
get2Params ( readMode1, readMode2 ) state =
    Debug.todo "blah"


get3 : State -> Result String ( Int, Int, Int )
get3 { memory, pointer } =
    case
        ( Array.get (pointer + 1) memory
        , Array.get (pointer + 2) memory
        , Array.get (pointer + 3) memory
        )
    of
        ( Just x, Just y, Just z ) ->
            Ok ( x, y, z )

        _ ->
            Err "Memory out of bounds"


{-| opcode: 1
-}
add : Instruction
add state =
    let
        { memory, pointer } =
            state

        xResult =
            readParam PositionMode memory (pointer + 1)

        yResult =
            readParam PositionMode memory (pointer + 2)

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
                    { memory = Array.set outputPointer (x + y) memory
                    , pointer = pointer + 4
                    }


{-| opcode: 2
-}
mult : Instruction
mult state =
    let
        { memory, pointer } =
            state

        xResult =
            readParam PositionMode memory (pointer + 1)

        yResult =
            readParam PositionMode memory (pointer + 2)

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
                    { memory = Array.set outputPointer (x * y) memory
                    , pointer = pointer + 4
                    }


{-| opcode: 3
-}
input : Instruction
input state =
    let
        { memory, pointer } =
            state

        -- For Day05, this is ALWAYS the only input.
        inputVal =
            1

        outputPointerResult =
            Array.get (pointer + 1) memory
                |> Result.fromMaybe (errorStringWithInt "Parameter out of bounds" (pointer + 1))
    in
    case outputPointerResult of
        Err a ->
            Err a

        Ok outputPointer ->
            if outputPointer >= Array.length memory || outputPointer < 0 then
                errorWithInt "Invalid output pointer: Can't write to address" outputPointer

            else
                Ok
                    { memory = Array.set outputPointer inputVal memory
                    , pointer = pointer + 2
                    }


{-| opcode: 4
-}
output : Instruction
output state =
    let
        { memory, pointer } =
            state

        xResult =
            readParam PositionMode memory (pointer + 1)
    in
    case xResult of
        Err a ->
            Err a

        Ok x ->
            Debug.log "output" x
                |> always (Ok { memory = memory, pointer = pointer + 2 })


{-| opcode: 99
-}
halt : Instruction
halt _ =
    Err "halt"
