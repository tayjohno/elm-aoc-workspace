module LazyMatrix exposing (LazyMatrix, get, new, set, size, toMatrix)

{- LazyMatrix uses a dictionary behind the scenes and assumes that all
   values are still default until
-}

import Coordinate exposing (Coordinate)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Matrix exposing (Matrix)


type alias LazyMatrix a =
    { data : Dict Coordinate a
    , default : a
    }


toMatrix : LazyMatrix a -> Matrix a
toMatrix aLazyMatrix =
    let
        xMin =
            aLazyMatrix.data
                |> Dict.keys
                |> List.map Tuple.first
                |> List.minimum
                |> Maybe.withDefault 0

        xMax =
            aLazyMatrix.data
                |> Dict.keys
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0

        yMin =
            aLazyMatrix.data
                |> Dict.keys
                |> List.map Tuple.second
                |> List.minimum
                |> Maybe.withDefault 0

        yMax =
            aLazyMatrix.data
                |> Dict.keys
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0

        data =
            aLazyMatrix.data
                |> Dict.mapKeys (\( x, y ) -> ( x - xMin, y - yMin ))
    in
    Dict.foldl
        (\key val matrix -> Matrix.set key val matrix)
        (Matrix.empty ( xMax - xMin + 1, yMax - yMin + 1 ) aLazyMatrix.default)
        data


new : a -> LazyMatrix a
new default =
    { data = Dict.empty
    , default = default
    }


get : Coordinate -> LazyMatrix a -> a
get coordinate { data, default } =
    data
        |> Dict.get coordinate
        |> Maybe.withDefault default


set : Coordinate -> a -> LazyMatrix a -> LazyMatrix a
set coordinate value lazyMatrix =
    if get coordinate lazyMatrix == value then
        -- be lazy, don't update
        lazyMatrix

    else
        { lazyMatrix
            | data = lazyMatrix.data |> Dict.insert coordinate value
        }


size : LazyMatrix a -> Int
size lazyMatrix =
    lazyMatrix.data |> Dict.size
