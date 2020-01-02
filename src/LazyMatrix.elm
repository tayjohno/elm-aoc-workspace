module LazyMatrix exposing (LazyMatrix, get, new, set, size)

{- LazyMatrix uses a dictionary behind the scenes and assumes that all
   values are still default until
-}

import Coordinate exposing (Coordinate)
import Dict exposing (Dict)


type alias LazyMatrix a =
    { data : Dict Coordinate a
    , default : a
    }


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