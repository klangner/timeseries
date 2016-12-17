# Benchmarking the library

Install criterion package

```sh
cabal update
cabal install -j --disable-tests criterion
```

Create folder for the temporary files

```sh
mkdir temp
```

Build the benchmarking application.

```sh
ghc -O2 -rtsopts --make -isrc -outputdir temp -o temp/bench benchmark/Benchmarks.hs
```

Run performance tests

```sh
temp/bench --output temp/bench.html +RTS -s
```

Browse the results

```sh
firefox output/bench.html
```

## Memory usage

Latest benchmark:

```
benchmarking Basic operations/size
time                 10.13 ms   (10.08 ms .. 10.18 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.09 ms   (10.08 ms .. 10.11 ms)
std dev              42.30 μs   (28.59 μs .. 68.95 μs)

benchmarking Basic operations/valueAt
time                 14.03 ms   (13.63 ms .. 14.40 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 13.70 ms   (13.62 ms .. 13.87 ms)
std dev              291.6 μs   (165.2 μs .. 389.0 μs)

benchmarking Basic operations/slice
time                 27.88 ms   (27.22 ms .. 28.46 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 28.06 ms   (27.80 ms .. 28.40 ms)
std dev              658.8 μs   (534.0 μs .. 826.1 μs)

benchmarking Basic operations/max
time                 63.98 ms   (54.32 ms .. 74.36 ms)
                     0.921 R²   (0.821 R² .. 0.989 R²)
mean                 60.73 ms   (54.63 ms .. 66.93 ms)
std dev              11.03 ms   (6.290 ms .. 16.52 ms)
variance introduced by outliers: 65% (severely inflated)

benchmarking Basic operations/fmap
time                 38.21 ms   (36.57 ms .. 39.39 ms)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 40.10 ms   (39.19 ms .. 40.88 ms)
std dev              1.738 ms   (1.443 ms .. 2.036 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Group operation/rolling
time                 256.2 ms   (252.3 ms .. 259.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 258.1 ms   (257.0 ms .. 258.8 ms)
std dev              1.172 ms   (672.1 μs .. 1.655 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking IO/load CSV
time                 322.4 ms   (304.9 ms .. 340.3 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 317.0 ms   (312.1 ms .. 320.1 ms)
std dev              4.945 ms   (2.185 ms .. 6.905 ms)
variance introduced by outliers: 16% (moderately inflated)

  81,136,902,096 bytes allocated in the heap
   5,005,200,520 bytes copied during GC
     192,711,328 bytes maximum residency (114 sample(s))
       2,856,336 bytes maximum slop
             538 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     155604 colls,     0 par    2.788s   2.780s     0.0000s    0.0047s
  Gen  1       114 colls,     0 par    2.555s   2.557s     0.0224s    0.1475s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   37.089s  ( 37.119s elapsed)
  GC      time    5.344s  (  5.337s elapsed)
  EXIT    time    0.002s  (  0.002s elapsed)
  Total   time   42.437s  ( 42.458s elapsed)

  %GC     time      12.6%  (12.6% elapsed)

  Alloc rate    2,187,603,620 bytes per MUT second

  Productivity  87.4% of total user, 87.4% of total elapsed
```

