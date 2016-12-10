{-|
Module:      Data.TimeSeries.Series
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Definition and basic operation on Series.
-}

module Data.TimeSeries.Series
    ( Index
    , DataValues
    , Series
    , series
    , size
    ) where

import qualified Prelude as P


type Timestamp = P.Integer
type Index = [Timestamp]
type DataValues = [P.Double]

data Series = Series Index DataValues


-- | Create new series
series :: Index -> DataValues -> Series
series idx vs = Series idx vs


-- | Get series size
size :: Series -> P.Int
size (Series idx _) = P.length idx
