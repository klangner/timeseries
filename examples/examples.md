# Examples

  * PlotSeries - Plot sample Time Series.
  * Smooth - Plot original and smoothed series


## Install chart package

```sh
cabal update
cabal install chart-diagrams
```

## Compile and run examples

```bash
mkdir temp
ghc -O2 --make -isrc -outputdir out -o out/plot-series examples/PlotSeries.hs
```

```bash
temp/plot-series
```