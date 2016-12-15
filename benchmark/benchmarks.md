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
time                 8.760 ms   (8.708 ms .. 8.797 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.924 ms   (8.879 ms .. 8.979 ms)
std dev              141.1 μs   (109.6 μs .. 202.0 μs)

benchmarking Basic operations/valueAt
time                 13.35 ms   (13.29 ms .. 13.40 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.45 ms   (13.40 ms .. 13.53 ms)
std dev              154.5 μs   (95.31 μs .. 220.3 μs)

benchmarking Basic operations/slice
time                 25.47 ms   (25.26 ms .. 25.71 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.41 ms   (25.35 ms .. 25.51 ms)
std dev              171.2 μs   (110.5 μs .. 241.7 μs)

benchmarking Basic operations/load CSV
time                 247.1 ms   (236.4 ms .. 254.7 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 254.6 ms   (251.1 ms .. 261.1 ms)
std dev              6.262 ms   (180.5 μs .. 7.533 ms)
variance introduced by outliers: 16% (moderately inflated)

  34,745,798,992 bytes allocated in the heap
   2,354,784,752 bytes copied during GC
     132,423,528 bytes maximum residency (60 sample(s))
       3,170,616 bytes maximum slop
             292 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     66723 colls,     0 par    0.709s   0.704s     0.0000s    0.0013s
  Gen  1        60 colls,     0 par    0.794s   0.797s     0.0133s    0.1002s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   22.487s  ( 22.515s elapsed)
  GC      time    1.504s  (  1.501s elapsed)
  EXIT    time    0.002s  (  0.002s elapsed)
  Total   time   23.998s  ( 24.018s elapsed)

  %GC     time       6.3%  (6.2% elapsed)

  Alloc rate    1,545,151,064 bytes per MUT second

  Productivity  93.7% of total user, 93.7% of total elapsed
```

