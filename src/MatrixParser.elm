module MatrixParser exposing (parse)

import Matrix exposing (Matrix)


{-| Given a string representation, and a method for decoding the
characters, create a matrix.
-}
parse : a -> (Char -> a) -> String -> Matrix a
parse default function string =
    string
        |> String.lines
        |> List.map
            -- Each row to a list of characters, then map to tiles
            (String.toList
                >> List.map function
            )
        |> Matrix.fromRows default
