
module Data.TimeSeries.Sessions
    ( Session
    , find
    ) where

import Data.Time (UTCTime)
import Data.TimeSeries.Series (Series)


-- | Session is a time range of a single event
--   Some examples:
--     * Session with web application
--     * Rain event
data Session = Session { sessionStart :: UTCTime
                       , sessionEnd :: UTCTime }
                       deriving (Eq, Show)


-- | Find all session in given series
find :: Series a -> [Session]
find _ = []