# Examples

The haskell files in this directory show some usage of timeseries library.

To build these examples using the in-tree version of timeseries, run:

```
caba sandbox init
cabal sandbox add-source ../src
cabal install --only-dependencies
cabal build
```


## PlotSeries.hs

Usage:

```
PlotSeries data/co2.csv
```

This example shows how to load Time Series from csv file and plot it using
the chart library.

![Series plot](https://rawgit.com/klangner/timeseries/master/examples/images/co2.svg)


## Resample.hs

```
Resample data/co2.csv
```

Resampling time series. Here is the same TimeSeries resampled with 1 year resolution.

![Resampled](https://rawgit.com/klangner/timeseries/master/examples/images/co2_resample.svg)


## Smooth.hs

```
Smooth data/co2.csv
```

Smoothing time series with rolling window of the 3 years size.

![Smoothed](https://rawgit.com/klangner/timeseries/master/examples/images/co2_smooth.svg)


## Sessions

```
Sessions data/rain.csv
```

This example show how to find all rain events using sessions functionality.
Rain event is defined as continuous rainfall with the breaks shorter then 6 hours.

Some found sessions:
```
(Session {sessionStart = 2014-09-03 10:00:00 UTC, sessionEnd = 2014-09-04 02:00:00 UTC},34.42)
(Session {sessionStart = 2006-06-13 15:00:00 UTC, sessionEnd = 2006-06-15 00:00:00 UTC},34.66)
(Session {sessionStart = 2012-06-22 20:00:00 UTC, sessionEnd = 2012-06-24 21:00:00 UTC},39.019999999999996)
(Session {sessionStart = 2008-07-01 13:00:00 UTC, sessionEnd = 2008-07-02 02:00:00 UTC},40.9)
(Session {sessionStart = 2013-06-18 19:00:00 UTC, sessionEnd = 2013-06-22 06:00:00 UTC},81.85000000000001)
```


