module Intcode.Int exposing (fromBool)


fromBool : Bool -> Int
fromBool bool =
    if bool then
        1

    else
        0
