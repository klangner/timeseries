{-|
Module:      Data.TimeSeries.Sessions
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Operation on sessions.
-}

module Data.TimeSeries.Sessions
    ( Session(..)
    , find
    ) where


import Prelude
import Data.Time (UTCTime)
import Data.TimeSeries.Series (TimeSeries, toList)

import Data.TimeSeries.Time(TimeResolution, nextTime)


-- | Session is a time range of a single event
--   Some examples:
--
--     * Session with web application
--
--     * Rain event
--
data Session = Session { sessionStart :: UTCTime
                       , sessionEnd :: UTCTime }
                       deriving (Eq, Show)


-- | Find all session in a given Time Series
find :: TimeResolution -> TimeSeries Bool -> [Session]
find dt ts = map (uncurry Session) xs'
    where
        xs = findChanges False (toList ts)
        xs' = join dt (cnv xs)


-- Helper function for finding all places where value changes from True to False or vice versa
findChanges :: Bool -> [(a, Bool)] -> [a]
findChanges _ [] = []
findChanges True [x] = [fst x]
findChanges v (x:xs) | v == snd x = findChanges v xs
                     | otherwise = fst x : findChanges (snd x) xs

-- Convert list of changes into tuple2
cnv :: [a] -> [(a, a)]
cnv [] = []
cnv [_] = []
cnv (x:y:z) = (x, y) : cnv z

-- Join events close to each other into single session
join :: TimeResolution -> [(UTCTime, UTCTime)] -> [(UTCTime, UTCTime)]
join _ [] = []
join _ [x] = [x]
join dt (x:y:z) = if nextTime dt (snd x) < fst y
                    then x:join dt (y:z)
                    else join dt ((fst x, snd y):z)
