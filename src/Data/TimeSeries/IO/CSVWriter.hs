{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TimeSeries.IO.CSVWriter( saveCSV ) where


import           Data.Csv

import           Data.TimeSeries.Series ( Series )


-- | Load data from CSV file and create Time Series from it
-- As a first argument provide function to convert date from ByteString to UTCTime
saveCSV :: Series -> FilePath -> IO ()
saveCSV ts filePath = do
    putStrLn filePath
