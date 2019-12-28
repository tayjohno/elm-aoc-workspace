module Coordinate exposing (Coordinate, distance, down, eightNeighbors, fourNeighbors, heading, left, right, up)

{-| ( x, y )
-}


type alias Coordinate =
    ( Int, Int )


up ( x, y ) =
    ( x, y - 1 )


left ( x, y ) =
    ( x - 1, y )


right ( x, y ) =
    ( x + 1, y )


down ( x, y ) =
    ( x, y + 1 )


{-| Returns the direction (in radians) from one coordinate to another.

This will always return a positive value (0-2π)

-}
heading : Coordinate -> Coordinate -> Float
heading ( fromX, fromY ) ( toX, toY ) =
    let
        dx =
            toX - fromX |> toFloat

        -- This subtraction is intentionally reversed to ensure that "up" is equal to 0
        dy =
            fromY - toY |> toFloat

        dir =
            atan2 dx dy
    in
    if dir < 0 then
        dir + (2 * pi)

    else
        dir


distance : Coordinate -> Coordinate -> Float
distance ( fromX, fromY ) ( toX, toY ) =
    let
        dx =
            toX - fromX |> toFloat

        dy =
            toY - fromY |> toFloat
    in
    sqrt (dx ^ 2 + dy ^ 2)


fourNeighbors : Coordinate -> List Coordinate
fourNeighbors coordinate =
    [ up, left, right, down ]
        |> List.map (\direction -> direction coordinate)


eightNeighbors : Coordinate -> List Coordinate
eightNeighbors coordinate =
    let
        allDirections =
            [ up >> left
            , up
            , up >> right
            , left
            , right
            , down >> left
            , down
            , down >> right
            ]
    in
    allDirections
        |> List.map (\direction -> direction coordinate)
