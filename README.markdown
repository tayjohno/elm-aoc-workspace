### Developing

To add your answers, simply open up the file for the Day you're working on, for example
`src/Day01.elm` for the *Day 1 Problem*.

There are two methods in there. To solve part one, implment the method `partOne`. When Every answer
should return a type of Answer (see `src/Answer.elm`). This can be `Solved "answer string"` for when
you have a solution, or `Unsolved` if you haven't figured it out yet. Also, if you find a solution
that just takes too long to calculate, feel free to mark it as `Faked "abc"` so that you know that
the answer isn't being calculated from scratch each time.

```elm
{-| Calculate the answer (integer) and convert it to an Answer type (Solved String).
partOne =
    input
        |> calculateTheAnswer
        |> Answer.fromInt

{-| faked with the solution so it doesn't slow down the website or tests -}
partTwo =
    "12345"
        |> Faked
```

### Testing

Once you've confirmed the answer to a problem, you can add a test for that answer in the `tests/Example.elm` file. Then if you change anything you can verify that it still returns the right answer by running the tests.

```bash
# This is the same as `yarn elm-test`
yarn test
```

### Running

To see the output, run the elm make command below and then open the `index.html`.

```bash
# This is the same as `yarn elm make src/Answers.elm --output=elm.js`
yarn make
```

Even faster, you can run `yarn launch` which will open `index.html` _for you_ after it has finished building.
