# Examples

  * PlotSeries - Plot sample Time Series. 


## Install chart package

```sh
cabal update
cabal install chart-diagrams
```

## Compile and run examples

```bash
ghc -O2 --make -i../src -outputdir ../temp -o ../temp/plot-series PlotSeries.hs 
```

```bash
../temp/plot-series
```