{-|
Module:      Data.TimeSeries.Stats
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Calculate Series statistics.
-}

module Data.TimeSeries.Stats
    ( mean
    , variance
    ) where

import Prelude hiding (max, min)
import Data.TimeSeries.Series (TimeSeries)


-- | Return Time Series mean
mean :: Fractional a => TimeSeries a -> a
mean xs = sum xs / fromIntegral (length xs)


-- | Calculate Time Series variance
variance :: Floating a => TimeSeries a -> a
variance xs = sum (fmap (\x -> (x-mu)**2) xs) / fromIntegral (length xs)
    where mu = mean xs
