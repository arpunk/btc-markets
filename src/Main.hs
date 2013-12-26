{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger

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

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)

import Control.Monad.Trans.Resource (runResourceT, ResourceT)

import System.Environment
import System.Locale

import Market

connStr :: ConnectionString
connStr = "host=localhost dbname=btc-markets user=arpunk password='' port=5432"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Trade
    ts     UTCTime
    price  Double
    amount Double
    deriving Show
|]

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT $ runResourceT . withPostgresqlConn connStr . runSqlConn . (runMigration migrateAll >>) $ query

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["import-trading-data", market, currency] -> do
      putStrLn $ "importing trades for market " ++ market ++ " (" ++ currency ++ ")"
      case market of
        "MtGox" -> importHistoricalData MtGox (toCurrency currency)
        "BitStamp" -> importHistoricalData BitStamp (toCurrency currency)
        _ -> error "No such market"

    _ -> return ()

importHistoricalData :: Market -> Currency -> IO ()
importHistoricalData m c = do
  datadump <- BS.readFile $ marketFile m c

  case parseCSV defCSVSettings datadump of
    Left e -> error e
    Right rs -> void $
      forM_ rs $ \(ts':price':amount') -> do
        let ts = fromJust $ parseTime defaultTimeLocale "%s" $ S.unpack ts'
            (price, _) = fromJust $ readDouble price'
            (amount, _) = fromJust $ readDouble $ amount' !! 0

        trade <- (runDb . insert) $ Trade ts price amount

        putStrLn $ show trade
