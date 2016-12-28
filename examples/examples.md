# Examples

The haskell files in this directory show some usage of timeseries library.


## PlotSeries.hs

This example shows how to load Time Series from csv file and plot it using
the chart library.

![Series plot](https://rawgit.com/klangner/timeseries/master/examples/images/co2.svg)


# Running the examples

Install packages required by examples:

```sh
cabal update
cabal install chart-diagrams
```

Run example:
```bash
mkdir out
runhaskell -isrc examples/PlotSeries.hs
```
