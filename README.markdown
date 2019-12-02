### Developing

To add your answers, simply open up the file for the Day you're working on, for example
`src/Day01.elm` for the *Day 1 Problem*.

There are two methods in there. To solve part one, implment the method `partOne`. When Every answer
should return a type of Answer (see `src/Answer.elm`). This can be `Solved "answer string"` for when
you have a solution, or `Unsolved` if you haven't figured it out yet. Also, if you find a solution
that just takes too long to calculate, feel free to mark it as `Faked "abc"` so that you know that
the answer isn't being calculated from scratch each time.

### Running

To see the output, run the elm make command below and then open the `index.html`.

```
elm make src/Answers.elm -- --output=elm.js
```
