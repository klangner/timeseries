[![Build Status](https://travis-ci.org/klangner/timeseries.svg?branch=master)](https://travis-ci.org/klangner/timeseries)
[![Hackage](https://img.shields.io/hackage/v/timeseries.svg)](https://hackage.haskell.org/package/timeseries)

# Welcome to Haskell Time Series library

Library for processing Time Series data.


# Features

  * [x] Reading and writing from CSV file
  * [x] Slice series
  * [x] Mapping over Series
  * [x] Folding Series
  * [x] Rolling window
  * [x] Resampling and groupBy
  * Calculate statistics
    * [x] min, max
    * [x] mean, variance, standard deviation
  * [x] Finding sessions (periods of activity)


## Installation

```sh
stack setup
stack build
stack test
```


# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/klangner/timeseries/issues).

Master [git repository](http://github.com/klangner/timeseries):

* `git clone https://github.com/klangner/timeseries.git`


# Redistributing

timeseries source code is distributed under the BSD3 License.
