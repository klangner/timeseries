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
benchmarking Basic operations/size
time                 8.922 ms   (8.875 ms .. 8.965 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.049 ms   (9.017 ms .. 9.085 ms)
std dev              96.36 μs   (81.10 μs .. 133.0 μs)

benchmarking Basic operations/valueAt
time                 12.43 ms   (12.39 ms .. 12.48 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.46 ms   (12.43 ms .. 12.49 ms)
std dev              71.06 μs   (58.53 μs .. 91.38 μs)

benchmarking Basic operations/slice
time                 25.12 ms   (25.01 ms .. 25.23 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.09 ms   (25.03 ms .. 25.17 ms)
std dev              148.4 μs   (111.3 μs .. 195.6 μs)

benchmarking Basic operations/apply
time                 33.89 ms   (33.81 ms .. 33.95 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 33.89 ms   (33.84 ms .. 33.96 ms)
std dev              111.6 μs   (68.27 μs .. 185.5 μs)

benchmarking IO/load CSV
time                 249.9 ms   (246.0 ms .. 254.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 252.7 ms   (252.0 ms .. 253.2 ms)
std dev              776.0 μs   (375.4 μs .. 1.089 ms)
variance introduced by outliers: 16% (moderately inflated)

  76,954,313,136 bytes allocated in the heap
   2,465,206,696 bytes copied during GC
     132,423,744 bytes maximum residency (61 sample(s))
       2,784,104 bytes maximum slop
             292 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     147553 colls,     0 par    0.862s   0.850s     0.0000s    0.0009s
  Gen  1        61 colls,     0 par    0.828s   0.829s     0.0136s    0.0986s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   27.623s  ( 27.662s elapsed)
  GC      time    1.691s  (  1.680s elapsed)
  EXIT    time    0.002s  (  0.002s elapsed)
  Total   time   29.320s  ( 29.343s elapsed)

  %GC     time       5.8%  (5.7% elapsed)

  Alloc rate    2,785,849,436 bytes per MUT second

  Productivity  94.2% of total user, 94.2% of total elapsed
```

