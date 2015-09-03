## Barista

White paper [here](https://gist.github.com/Cixelyn/775c557109d638a34988)

---

To build: 
```
$ cabal install
$ cabal run
```

To run tests, first interactively run `Test.hs` using `ghci`, and then invoke the `runtests` function
```bash
$ ghci Test.hs
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling Types            ( Types.hs, interpreted )
[2 of 3] Compiling BaristaParser    ( BaristaParser.hs, interpreted )
[3 of 3] Compiling Test             ( Test.hs, interpreted )
Ok, modules loaded: Test, BaristaParser, Types.
λ: runtests
Cases: 10  Tried: 10  Errors: 0  Failures: 0
Counts {cases = 10, tried = 10, errors = 0, failures = 0}
```
