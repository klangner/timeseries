[![Build Status](https://travis-ci.org/klangner/timeseries.svg?branch=master)](https://travis-ci.org/klangner/timeseries)
[![Hackage](https://img.shields.io/hackage/v/timeseries.svg)](https://hackage.haskell.org/package/timeseries)

# Welcome to Haskell Time Series library

Implemented functionality:

  * Reading and writing from CSV file
  * Base operations on time series:
    * get value
    * slice
  * Mapping and folding Series
  * Rolling window, Resampling and groupBy functions with time resolution
  * Calculate statistics
    * min, max
    * median, variance, standard deviation
  * Finding sessions (periods of activity)


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
