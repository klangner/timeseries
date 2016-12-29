# Benchmarking the library

Install criterion package

```sh
cabal update
cabal install -j --disable-tests criterion
```

Create folder for the temporary files

```sh
mkdir out
```

Build the benchmarking application.

```sh
ghc -O2 -rtsopts --make -isrc -outputdir out -o out/bench benchmark/Benchmarks.hs
```

Run performance tests

```sh
out/bench --output out/bench.html +RTS -s
```

Browse the results

```sh
firefox out/bench.html
```

## Memory usage

Latest benchmark:

```
benchmarking Basic operations/size
time                 13.14 ms   (13.10 ms .. 13.18 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.15 ms   (13.13 ms .. 13.19 ms)
std dev              71.99 μs   (47.02 μs .. 116.3 μs)

benchmarking Basic operations/valueAt
time                 16.79 ms   (14.91 ms .. 18.67 ms)
                     0.969 R²   (0.955 R² .. 1.000 R²)
mean                 15.11 ms   (14.79 ms .. 15.89 ms)
std dev              1.133 ms   (330.8 μs .. 1.826 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Basic operations/slice
time                 28.60 ms   (26.63 ms .. 30.11 ms)
                     0.985 R²   (0.960 R² .. 0.997 R²)
mean                 30.63 ms   (29.70 ms .. 32.69 ms)
std dev              2.763 ms   (1.110 ms .. 5.052 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Basic operations/max
time                 61.46 ms   (49.21 ms .. 70.70 ms)
                     0.911 R²   (0.779 R² .. 0.978 R²)
mean                 63.76 ms   (56.57 ms .. 70.46 ms)
std dev              12.33 ms   (8.458 ms .. 20.30 ms)
variance introduced by outliers: 65% (severely inflated)

benchmarking Basic operations/fmap
time                 37.17 ms   (37.09 ms .. 37.24 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.20 ms   (37.11 ms .. 37.27 ms)
std dev              151.6 μs   (94.57 μs .. 245.2 μs)

benchmarking Group operation/rolling
time                 248.8 ms   (247.1 ms .. 250.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 247.8 ms   (247.1 ms .. 248.3 ms)
std dev              710.9 μs   (333.5 μs .. 928.7 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Group operation/resampling
time                 479.7 μs   (477.7 μs .. 482.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 480.7 μs   (479.5 μs .. 482.3 μs)
std dev              4.604 μs   (3.190 μs .. 6.698 μs)

benchmarking IO/load CSV
time                 242.6 ms   (239.2 ms .. 245.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 244.8 ms   (244.2 ms .. 245.3 ms)
std dev              709.0 μs   (320.3 μs .. 850.0 μs)
variance introduced by outliers: 16% (moderately inflated)
```

