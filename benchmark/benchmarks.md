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
ghc -O2 -rtsopts --make -i../src -outputdir output -o output/bench Benchmarks.hs
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

Latest benchmark:

```
time                 8.768 ms   (8.730 ms .. 8.797 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.871 ms   (8.842 ms .. 8.905 ms)
std dev              90.71 μs   (71.34 μs .. 129.3 μs)

benchmarking Basic operations/valueAt
time                 13.29 ms   (13.25 ms .. 13.34 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.31 ms   (13.28 ms .. 13.37 ms)
std dev              115.2 μs   (55.19 μs .. 201.4 μs)

benchmarking Basic operations/slice
time                 26.11 ms   (25.87 ms .. 26.24 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 26.22 ms   (26.05 ms .. 26.79 ms)
std dev              605.6 μs   (107.0 μs .. 1.180 ms)

benchmarking Basic operations/max
time                 137.5 ms   (114.9 ms .. 161.1 ms)
                     0.967 R²   (0.921 R² .. 0.996 R²)
mean                 146.2 ms   (136.3 ms .. 159.5 ms)
std dev              15.23 ms   (8.815 ms .. 21.87 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Basic operations/fmap
time                 36.48 ms   (36.36 ms .. 36.59 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 36.48 ms   (36.40 ms .. 36.60 ms)
std dev              198.0 μs   (150.4 μs .. 295.9 μs)

benchmarking IO/load CSV
time                 249.8 ms   (242.7 ms .. 256.1 ms)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 251.1 ms   (248.4 ms .. 254.0 ms)
std dev              3.207 ms   (1.482 ms .. 4.487 ms)
variance introduced by outliers: 16% (moderately inflated)

  84,231,043,648 bytes allocated in the heap
   5,989,273,824 bytes copied during GC
     216,573,016 bytes maximum residency (76 sample(s))
       2,797,000 bytes maximum slop
             575 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     158925 colls,     0 par    3.245s   3.233s     0.0000s    0.0084s
  Gen  1        76 colls,     0 par    2.367s   2.368s     0.0312s    0.1381s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   29.516s  ( 29.535s elapsed)
  GC      time    5.612s  (  5.601s elapsed)
  EXIT    time    0.002s  (  0.002s elapsed)
  Total   time   35.134s  ( 35.138s elapsed)

  %GC     time      16.0%  (15.9% elapsed)

  Alloc rate    2,853,732,982 bytes per MUT second

  Productivity  84.0% of total user, 84.0% of total elapsed
```

