{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module:      Data.TimeSeries.IO.CSV
Copyright:   (c) 2016 Krzysztof Langner
License:     BSD3
Stability:   experimental
Portability: portable
Reading Time Series from CSV file
-}
module Data.TimeSeries.IO.CSV
    ( HasHeader(..)
    , load
    )where


import Prelude
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import           Data.Time (UTCTime)

import           Data.TimeSeries ( TimeSeries
                                 , ts
                                 , emptySeries
                                 )

-- | Define if csv file has header
data HasHeader = HasHeader | NoHeader

-- | Load data from CSV file and create Time Series from it
-- As a first argument provide function to convert date from ByteString to UTCTime
load :: HasHeader -> (T.Text -> UTCTime) -> FilePath -> IO TimeSeries
load hasHeader ft filePath = do
    csvData <- BS.readFile filePath
    case CSV.decode (toCSVHeader hasHeader) csvData of
        Left err -> do
            _ <- putStrLn err
            return emptySeries

        Right vs -> do
            let vs' = V.map (parseLine ft) vs
            let (idx, vs'') = V.unzip vs'
            return $ ts idx vs''


toCSVHeader :: HasHeader -> CSV.HasHeader
toCSVHeader HasHeader = CSV.HasHeader
toCSVHeader _ = CSV.NoHeader

parseLine :: (T.Text -> UTCTime) -> (T.Text, Float) -> (UTCTime, Float)
parseLine ft (date, value) = (ft date, value)