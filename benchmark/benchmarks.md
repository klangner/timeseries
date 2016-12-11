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

## Memory usage

For time series with 1M data points:
```
   1,427,244,008 bytes allocated in the heap
     369,940,160 bytes copied during GC
      80,176,216 bytes maximum residency (12 sample(s))
      11,539,240 bytes maximum slop
             159 MB total memory in use (0 MB lost due to fragmentation)
```

