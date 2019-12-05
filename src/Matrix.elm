module Matrix exposing (Matrix, Size, allCoordinates, allCoordinatesHelper, customPrint, eightNeighbors, empty, findAll, fromRows, get, locations, map, prettyPrint, set, toRows, toString, transformElement)

import Array exposing (Array)
import Coordinate exposing (Coordinate, eightNeighbors)


type alias Matrix a =
    { data : Array a, size : Size }


type alias Size =
    ( Int, Int )


empty : Size -> a -> Matrix a
empty size defaultVal =
    { data = Array.fromList (List.repeat ((\( w, h ) -> w * h) size) defaultVal)
    , size = size
    }


set : Coordinate -> a -> Matrix a -> Matrix a
set ( x, y ) val matrix =
    let
        ( width, height ) =
            matrix.size
    in
    if isWithin ( width, height ) ( x, y ) then
        -- Within bounds (insert)
        { data = Array.set (x + width * y) val matrix.data
        , size = matrix.size
        }

    else
        -- Out of bounds (do nothing)
        matrix


get : Coordinate -> Matrix a -> Maybe a
get ( x, y ) matrix =
    let
        ( width, height ) =
            matrix.size
    in
    if isWithin ( width, height ) ( x, y ) then
        matrix.data
            |> Array.get (x + width * y)

    else
        Nothing


{-| Return all the locations of a given item in the matrix
-}
locations : (a -> Bool) -> Matrix a -> List Coordinate
locations aMatcher aMatrix =
    aMatrix.data
        |> Array.indexedMap
            (\n a ->
                if aMatcher a then
                    Just (indexToCoordinate n aMatrix)

                else
                    Nothing
            )
        |> Array.foldl
            (\maybeC acc ->
                case maybeC of
                    Nothing ->
                        acc

                    Just a ->
                        a :: acc
            )
            []


transformElement : Coordinate -> (a -> a) -> Matrix a -> Matrix a
transformElement coordinate transformation matrix =
    let
        value =
            get coordinate matrix
    in
    case value of
        Nothing ->
            matrix

        Just a ->
            set coordinate (transformation a) matrix


toRows : Matrix a -> List (List a)
toRows matrix =
    let
        ( width, height ) =
            matrix.size
    in
    toRowsOfWidth width matrix.data []
        |> List.reverse


toRowsOfWidth : Int -> Array a -> List (List a) -> List (List a)
toRowsOfWidth width data list =
    case Array.length data of
        0 ->
            List.reverse list

        _ ->
            toRowsOfWidth
                width
                (Array.slice width (Array.length data) data)
                (Array.toList (Array.slice 0 width data) :: list)


fromRows : List (List a) -> a -> Matrix a
fromRows rows default =
    let
        size =
            ( rows |> List.map List.length |> List.maximum |> Maybe.withDefault 0
            , rows |> List.length
            )

        data =
            Array.fromList
                ((rows
                    |> List.map
                        (\row -> List.append row (List.repeat (Tuple.first size - List.length row) default))
                 )
                    |> List.concat
                )
    in
    { data = data
    , size = size
    }


allCoordinates : Matrix a -> List Coordinate
allCoordinates matrix =
    allCoordinatesHelper 0 matrix.size


allCoordinatesHelper : Int -> Size -> List Coordinate
allCoordinatesHelper index ( width, height ) =
    if index >= height then
        []

    else
        List.append (List.map (\a -> ( a, index )) (List.range 0 (width - 1))) (allCoordinatesHelper (index + 1) ( width, height ))


findAll : (a -> Bool) -> Matrix a -> List ( a, Coordinate )
findAll function matrix =
    locations function matrix
        |> List.map (\coordinate -> ( get coordinate matrix, coordinate ))
        |> List.map
            (\( maybeA, coordinate ) ->
                case maybeA of
                    Nothing ->
                        Debug.todo "Coordinate out of range"

                    Just a ->
                        ( a, coordinate )
            )


map : (Coordinate -> Matrix a -> a) -> Matrix a -> Matrix a
map function matrix =
    matrix
        |> allCoordinates
        |> List.foldl
            (\coordinate ->
                set coordinate (function coordinate matrix)
            )
            matrix


eightNeighbors : Matrix a -> Coordinate -> List a
eightNeighbors matrix coord =
    coord
        |> Coordinate.eightNeighbors
        |> List.filterMap (\c -> get c matrix)


toString : (a -> Char) -> Matrix a -> String
toString coersion matrix =
    toStringHelper coersion (toRows matrix)



-- PRIVATE METHODS


toStringHelper : (a -> Char) -> List (List a) -> String
toStringHelper coersion list =
    case list of
        [] ->
            ""

        head :: tail ->
            String.fromList (List.map coersion head) ++ "\n" ++ toStringHelper coersion tail


isWithin : Size -> Coordinate -> Bool
isWithin ( width, height ) ( x, y ) =
    x >= 0 && y >= 0 && x < width && y < height


prettyPrint : Matrix a -> Matrix a
prettyPrint matrix =
    matrix
        |> toRows
        |> List.reverse
        |> List.map (\a -> Debug.log "" a)
        |> always matrix


{-| A compact way to print out any matrix.

Must provide a way to map any instance of `a` to a single character.

-}
customPrint : (a -> Char) -> Matrix a -> Matrix a
customPrint function matrix =
    matrix
        |> customPrintHelper function "" 0
        |> always matrix



-- PRIVATE METHODS


customPrintHelper : (a -> Char) -> String -> Int -> Matrix a -> Matrix a
customPrintHelper function row index matrix =
    let
        ( width, height ) =
            matrix.size
    in
    if index >= width * height then
        Debug.log "" row |> always matrix

    else if modBy width index == 0 then
        let
            _ =
                if index /= 0 then
                    Just (Debug.log "" row)

                else
                    Nothing
        in
        customPrintHelper
            function
            ""
            (index + 1)
            matrix

    else
        customPrintHelper
            function
            (Array.get index matrix.data |> Maybe.map function |> Maybe.withDefault ' ' |> String.fromChar |> String.append row)
            (index + 1)
            matrix


{-| Convert an array index to a coordinate.
-}
indexToCoordinate : Int -> Matrix a -> Coordinate
indexToCoordinate int { size } =
    let
        ( width, _ ) =
            size

        x =
            remainderBy width int

        y =
            int // width
    in
    ( x, y )
