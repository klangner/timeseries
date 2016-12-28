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
    , std
    , variance
    ) where

import Prelude hiding (max, min)
import Data.TimeSeries.Series (Series)


-- | Return Time Series mean
mean :: Fractional a => Series a -> a
mean xs = sum xs / fromIntegral (length xs)


-- | Calculate Time Series variance
variance :: Floating a => Series a -> a
variance xs = sum (fmap (\x -> (x-mu)**2) xs) / fromIntegral (length xs)
    where mu = mean xs



-- | Calculate Time Series Standard Deviation
std :: Floating a => Series a -> a
std = sqrt . variance
