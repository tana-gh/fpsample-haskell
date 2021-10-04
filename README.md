# fpsample-haskell

## Haskell implementation for fpsample

This app gets data (`name` and `age` value) from specified server, and print the values into standard output.

## How to use

Execute following command, to run test.

```
$ cabal test
```

Execute following command, to run app.
(Required to run fpsample-mockserver before.)

```
$ cabal run fpsample-haskell -- --url http://localhost:8080/
```
