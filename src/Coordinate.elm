module Coordinate exposing (Coordinate, down, eightNeighbors, fourNeighbors, left, right, up)

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
