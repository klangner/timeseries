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
benchmarking Big series/size
time                 8.504 ms   (8.464 ms .. 8.533 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.593 ms   (8.564 ms .. 8.630 ms)
std dev              90.73 μs   (69.51 μs .. 134.5 μs)

benchmarking Big series/valueAt
time                 14.10 ms   (14.04 ms .. 14.16 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.08 ms   (14.04 ms .. 14.12 ms)
std dev              98.14 μs   (70.20 μs .. 151.0 μs)

benchmarking Big series/slice
time                 28.47 ms   (28.31 ms .. 28.73 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 28.33 ms   (28.25 ms .. 28.43 ms)
std dev              208.0 μs   (131.7 μs .. 299.1 μs)

  12,659,195,184 bytes allocated in the heap
     857,227,504 bytes copied during GC
     132,423,424 bytes maximum residency (13 sample(s))
       1,587,672 bytes maximum slop
             292 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     24245 colls,     0 par    0.170s   0.168s     0.0000s    0.0012s
  Gen  1        13 colls,     0 par    0.452s   0.452s     0.0348s    0.0982s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   17.756s  ( 17.762s elapsed)
  GC      time    0.622s  (  0.620s elapsed)
  EXIT    time    0.014s  (  0.014s elapsed)
  Total   time   18.396s  ( 18.397s elapsed)

  %GC     time       3.4%  (3.4% elapsed)

  Alloc rate    712,954,494 bytes per MUT second

  Productivity  96.6% of total user, 96.6% of total elapsed

```

