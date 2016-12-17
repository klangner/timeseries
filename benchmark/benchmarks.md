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

## The same data for Vector structure

It looks that functions like map, rolling takes more time. So it looks that this is not good data structure.

```
benchmarking Basic operations/size
time                 306.9 μs   (306.4 μs .. 307.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 307.3 μs   (306.8 μs .. 307.9 μs)
std dev              1.797 μs   (1.414 μs .. 2.487 μs)

benchmarking Basic operations/valueAt
time                 8.932 ms   (8.726 ms .. 9.172 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 8.648 ms   (8.513 ms .. 8.758 ms)
std dev              337.5 μs   (269.4 μs .. 451.4 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Basic operations/slice
time                 16.74 ms   (16.51 ms .. 17.09 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 16.42 ms   (16.20 ms .. 16.61 ms)
std dev              497.5 μs   (381.2 μs .. 652.2 μs)

benchmarking Basic operations/max
time                 70.15 ms   (62.63 ms .. 75.76 ms)
                     0.981 R²   (0.955 R² .. 0.996 R²)
mean                 68.27 ms   (61.21 ms .. 73.58 ms)
std dev              10.05 ms   (6.021 ms .. 18.85 ms)
variance introduced by outliers: 53% (severely inflated)

benchmarking Basic operations/fmap
time                 204.7 ms   (177.8 ms .. 219.2 ms)
                     0.993 R²   (0.982 R² .. 1.000 R²)
mean                 195.5 ms   (190.6 ms .. 200.7 ms)
std dev              6.881 ms   (4.723 ms .. 8.775 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Group operation/rolling
time                 606.6 ms   (275.7 ms .. 1.027 s)
                     0.948 R²   (0.857 R² .. 1.000 R²)
mean                 761.0 ms   (679.4 ms .. 836.9 ms)
std dev              126.8 ms   (0.0 s .. 131.5 ms)
variance introduced by outliers: 46% (moderately inflated)

benchmarking IO/load CSV
time                 353.2 ms   (316.7 ms .. 389.5 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 356.1 ms   (351.7 ms .. 360.3 ms)
std dev              7.089 ms   (0.0 s .. 7.286 ms)
variance introduced by outliers: 19% (moderately inflated)

  44,283,833,104 bytes allocated in the heap
  12,770,561,488 bytes copied during GC
     401,785,120 bytes maximum residency (177 sample(s))
      38,569,160 bytes maximum slop
             820 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     81683 colls,     0 par   11.035s  11.108s     0.0001s    0.0055s
  Gen  1       177 colls,     0 par    8.376s   8.464s     0.0478s    0.2379s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   27.523s  ( 27.665s elapsed)
  GC      time   19.410s  ( 19.572s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time   46.935s  ( 47.237s elapsed)

  %GC     time      41.4%  (41.4% elapsed)

  Alloc rate    1,608,979,143 bytes per MUT second

  Productivity  58.6% of total user, 58.3% of total elapsed

```