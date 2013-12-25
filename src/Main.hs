{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import Control.Applicative
import Control.Monad

import Data.Maybe

import Data.CSV.Conduit
import Data.CSV.Conduit.Parser.ByteString

import Data.Text (Text)
import Data.Time

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import Data.ByteString.Lex.Double

import           Control.Monad.Trans.Resource (runResourceT, ResourceT)

import System.Environment
import System.Locale

data Trade = Trade { tradeTs     :: UTCTime
                   , tradePrice  :: Double
                   , tradeAmount :: Double
                   } deriving Show

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["import-trading-data", market] -> do
      putStrLn $ "importing trades for market " ++ market
      case market of
        "MtGox" -> importHistoricalData "mtgoxUSD.csv"
        "BitStamp" -> importHistoricalData "bitstampUSD.csv"
        _ -> error "No such market"

    _ -> return ()

importHistoricalData :: FilePath -> IO ()
importHistoricalData file = do
  datadump <- BS.readFile $ "data/" ++ file

  case parseCSV defCSVSettings datadump of
    Left e -> error e
    Right rs -> void $
      forM_ rs $ \(ts':price':amount') -> do
        let ts = fromJust $ parseTime defaultTimeLocale "%s" $ S.unpack ts'
            (price, _) = fromJust $ readDouble price'
            (amount, _) = fromJust $ readDouble $ amount' !! 0
            trade = Trade ts price amount

        putStrLn $ show trade
