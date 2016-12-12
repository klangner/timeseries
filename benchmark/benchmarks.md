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
ghc -O -rtsopts --make -i../src -outputdir output -o output/bench Benchmarks.hs
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
time                 9.964 ms   (9.838 ms .. 10.12 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.936 ms   (9.899 ms .. 9.989 ms)
std dev              118.1 μs   (87.18 μs .. 168.3 μs)

benchmarking Big series/valueAt
time                 14.76 ms   (14.72 ms .. 14.82 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.75 ms   (14.72 ms .. 14.78 ms)
std dev              83.23 μs   (60.66 μs .. 111.0 μs)

benchmarking Big series/slice
time                 30.58 ms   (30.46 ms .. 30.71 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 30.63 ms   (30.56 ms .. 30.73 ms)
std dev              169.7 μs   (112.6 μs .. 274.4 μs)

  11,645,025,008 bytes allocated in the heap
     941,868,416 bytes copied during GC
     164,423,464 bytes maximum residency (13 sample(s))
       1,310,216 bytes maximum slop
             361 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     22284 colls,     0 par    0.165s   0.163s     0.0000s    0.0010s
  Gen  1        13 colls,     0 par    0.486s   0.486s     0.0374s    0.1212s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   17.533s  ( 17.537s elapsed)
  GC      time    0.651s  (  0.649s elapsed)
  EXIT    time    0.016s  (  0.016s elapsed)
  Total   time   18.204s  ( 18.202s elapsed)

  %GC     time       3.6%  (3.6% elapsed)

  Alloc rate    664,164,314 bytes per MUT second

  Productivity  96.4% of total user, 96.4% of total elapsed
```

