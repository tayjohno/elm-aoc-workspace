module Day04 exposing (..)

{--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a
password. The Elves had written the password on a sticky note, but someone
threw it out.

However, they do remember a few key facts about the password:

  - It is a six-digit number.
  - The value is within the range given in your puzzle input.
  - Two adjacent digits are the same (like 22 in 122345).
  - Going from left to right, the digits never decrease; they only ever
    increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:

  - 111111 meets these criteria (double 11, never decreases).
  - 223450 does not meet these criteria (decreasing pair of digits 50).
  - 123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input
meet these criteria?

--- Part Two ---

An Elf just remembered one more important detail: the two adjacent
matching digits are not part of a larger group of matching digits.

Given this additional criterion, but still ignoring the range rule, the
following are now true:

  - 112233 meets these criteria because the digits never decrease and all
    repeated digits are exactly two digits long.

  - 123444 no longer meets the criteria (the repeated 44 is part of a
    larger group of 444).

  - 111122 meets the criteria (even though 1 is repeated more than twice,
    it still contains a double 22).

How many different passwords within the range given in your puzzle input
meet all of the criteria?

-}

import Answer exposing (Answer(..))
import Regex exposing (Regex)


type alias Input =
    ( Int, Int )


partOne : () -> Answer String
partOne _ =
    let
        ( min, max ) =
            input
    in
    List.range min max
        |> List.map String.fromInt
        |> List.filter (isValidPassword passwordValidator1)
        |> List.length
        |> String.fromInt
        |> Solved


partTwo : () -> Answer String
partTwo _ =
    let
        ( min, max ) =
            input
    in
    List.range min max
        |> List.map String.fromInt
        |> List.filter (isValidPassword passwordValidator2)
        |> List.length
        |> String.fromInt
        |> Solved


input : Input
input =
    case rawInput |> String.split "-" of
        start :: end :: [] ->
            case ( start |> String.toInt, end |> String.toInt ) of
                ( Just a, Just b ) ->
                    ( a, b )

                _ ->
                    Debug.todo "Invalid input?"

        _ ->
            Debug.todo "Invalid input?"


rawInput =
    "245182-790572"


isValidPassword : (String -> Result String String) -> String -> Bool
isValidPassword passwordValidator password =
    case password |> passwordValidator of
        Ok _ ->
            True

        Err _ ->
            False


passwordValidator1 : String -> Result String String
passwordValidator1 password =
    Ok password
        -- These were never necessary, but fun to implement
        -- |> Result.andThen validateLength
        -- |> Result.andThen validateRange
        |> Result.andThen validateIncreasing
        |> Result.andThen validatePair


passwordValidator2 : String -> Result String String
passwordValidator2 password =
    Ok password
        -- These were never necessary, but fun to implement
        -- |> Result.andThen validateLength
        -- |> Result.andThen validateRange
        |> Result.andThen validateIncreasing
        |> Result.andThen validateExactPair


validateLength : String -> Result String String
validateLength password =
    if (password |> String.length) == 6 then
        Ok password

    else
        Err (password ++ " is not 6 characters")


validateRange : String -> Result String String
validateRange password =
    if password >= (input |> Tuple.first |> String.fromInt) && password <= (input |> Tuple.second |> String.fromInt) then
        Ok password

    else
        Err (password ++ " is not within input range")


{-| Ensure that there exists some side-by-side pair
-}
validatePair : String -> Result String String
validatePair password =
    if password |> Regex.contains digitPairRegex then
        Ok password

    else
        Err (password ++ " has no matching digit pairs.")


digitPairRegex : Regex
digitPairRegex =
    case Regex.fromString "([0-9])\\1" of
        Just regex ->
            regex

        _ ->
            Debug.todo "Invalid Regex"


{-| Ensure that there exists some side-by-side pair
-}
validateExactPair : String -> Result String String
validateExactPair password =
    case password |> String.toList of
        head :: tail ->
            if validateExactPairHelper head 1 tail then
                Ok password

            else
                Err (password ++ " doesn't have any matching sets of exactly 2")

        _ ->
            Err "Password 0 characters long?"


validateExactPairHelper : Char -> Int -> List Char -> Bool
validateExactPairHelper currentChar int charList =
    case charList of
        [] ->
            if int == 2 then
                True

            else
                False

        nextChar :: tail ->
            if nextChar == currentChar then
                validateExactPairHelper currentChar (int + 1) tail

            else if int == 2 then
                True

            else
                validateExactPairHelper nextChar 1 tail


{-| Ensure that there exists only increasing digits
-}
validateIncreasing : String -> Result String String
validateIncreasing password =
    let
        digits =
            password |> String.toList
    in
    if List.sort digits == digits then
        Ok password

    else
        Err (password ++ " is not sorted.")
