module Answer exposing (Answer(..), fromInt, fromMaybe, fromMaybeInt)


type Answer a
    = Solved a
    | Faked a -- For values which take too long to calculate. Just fake the answer, so it doesn't slow down your test suite.
    | Unsolved


{-| Convert a maybe into an answer.
-}
fromMaybe : Maybe String -> Answer String
fromMaybe =
    Maybe.map Solved
        >> Maybe.withDefault Unsolved


{-| Convert a maye int into an answer.
-}
fromMaybeInt : Maybe Int -> Answer String
fromMaybeInt =
    Maybe.map String.fromInt
        >> fromMaybe


fromInt : Int -> Answer String
fromInt =
    String.fromInt >> Solved
