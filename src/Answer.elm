module Answer exposing (Answer(..))


type Answer a
    = Solved a
    | Faked a -- For values which take too long to calculate. Just fake the answer, so it doesn't slow down your test suite.
    | Unsolved
