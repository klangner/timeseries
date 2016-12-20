{-# LANGUAGE OverloadedStrings #-}

module Data.TimeSeries.IO.CSVWriter( saveCSV ) where


import Prelude hiding (writeFile)
import Control.Arrow (first)
import Data.Csv
import Data.ByteString.Lazy (writeFile)
import Data.Text.Time ( formatISODateTime )

import Data.TimeSeries.Series ( Series
                              , toList )


-- | Load data from CSV file and create Time Series from it
-- As a first argument provide function to convert date from ByteString to UTCTime
saveCSV :: Series Double -> FilePath -> IO ()
saveCSV ts filePath = do
    let rs = map (first formatISODateTime) (toList ts)
    let rs' = encode rs
    writeFile filePath rs'
