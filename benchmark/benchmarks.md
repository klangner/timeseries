# Benchmarking the library

Install criterion package

```sh
cabal update
cabal install -j --disable-tests criterion
```

Create folder for the temporary files

```sh
cd benchamrk
mkdir output
```

Build the benchmarking application.

```sh
ghc -O -rtsopts --make -i../src -outputdir output -o output/bench Main.hs
```

Run performance tests

```sh
output/bench --output output/bench.html +RTS -s
```

Browse the results

```sh
firefox output/bench.html
```