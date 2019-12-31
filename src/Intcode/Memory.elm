module Intcode.Memory exposing (Memory, fromList, read, toList, write)

import Array exposing (Array)
import Array.Extra as Array


type Memory
    = Memory (Array Int)


fromList : List Int -> Memory
fromList =
    Array.fromList >> Memory


read : Int -> Memory -> Int
read pointer (Memory memory) =
    if pointer < 0 then
        Debug.todo "Accessing memory out of bounds"

    else
        Array.get pointer memory |> Maybe.withDefault 0


write : Int -> Int -> Memory -> Memory
write pointer value (Memory memory) =
    if pointer < 0 then
        Debug.todo "Accessing memory out of bounds"

    else if pointer >= Array.length memory then
        let
            resizedMemory =
                Array.resizelRepeat (pointer + 1) 0 memory
        in
        Memory (Array.set pointer value resizedMemory)

    else
        Memory (Array.set pointer value memory)


toList : Memory -> List Int
toList (Memory memory) =
    memory |> Array.toList
