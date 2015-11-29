CAP
===

Type checker for CAP.

Cabal >= 1.20 is required to compile and run the program. To install dependencies and compile:

```
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
```

Run tests with `cabal test`.

Launch an interactive interpreter with  `cabal repl`.

Examples of how to write expressions can be found in `test/TypecheckerTest.hs`.


