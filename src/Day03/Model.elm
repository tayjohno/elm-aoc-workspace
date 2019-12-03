module Day03.Model exposing (Direction(..), Segment)


type alias Segment =
    ( Direction, Int )


type Direction
    = Up
    | Down
    | Left
    | Right
