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
benchmarking Big series/size
time                 8.581 ms   (8.537 ms .. 8.622 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.728 ms   (8.686 ms .. 8.784 ms)
std dev              139.7 μs   (95.08 μs .. 212.1 μs)

benchmarking Big series/valueAt
time                 13.22 ms   (13.17 ms .. 13.27 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.23 ms   (13.20 ms .. 13.27 ms)
std dev              87.47 μs   (55.01 μs .. 160.0 μs)

benchmarking Big series/slice
time                 28.11 ms   (27.91 ms .. 28.21 ms)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 28.27 ms   (28.15 ms .. 28.87 ms)
std dev              430.9 μs   (86.02 μs .. 883.2 μs)

  12,749,572,504 bytes allocated in the heap
     857,260,200 bytes copied during GC
     132,423,504 bytes maximum residency (13 sample(s))
       1,658,960 bytes maximum slop
             292 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     24422 colls,     0 par    0.168s   0.166s     0.0000s    0.0011s
  Gen  1        13 colls,     0 par    0.446s   0.446s     0.0343s    0.0965s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   17.527s  ( 17.533s elapsed)
  GC      time    0.614s  (  0.612s elapsed)
  EXIT    time    0.014s  (  0.014s elapsed)
  Total   time   18.159s  ( 18.159s elapsed)

  %GC     time       3.4%  (3.4% elapsed)

  Alloc rate    727,415,864 bytes per MUT second

  Productivity  96.6% of total user, 96.6% of total elapsed
```

