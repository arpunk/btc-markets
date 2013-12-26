{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Market where

import Database.Persist.TH

data Currency = ARS | AUD | BRL | CAD | COP | CHF | CNY | CZK | DDK | EUR
              | GBP | HKD | ILS | INR | JPY | LTC | MXN | NOK | NZD | PLD
              | RUB | SEK | SGD | SSL | THB | USD | XRP | ZAR
              deriving (Show, Eq, Read, Bounded, Enum)

data Market = LocalBitcoins | BTCmarkets | MtGox | WeExchange | MercadoBitcoin
            | CanadianVirtualExchange | AsiaNexgen | BTCChina | Bitcurex
            | BitcoinDE | BTCe | CryptoTrade | Intersango | Justcoin
            | Ripple | RockTradingCompany | Vircurex | Bit2C | BitMe | BitNZ
            | FYBSE | Kapiton | FYBSG | VirWox | BitKonan | BitStamp | CampBX
            | ICBIT | BitX | BitMarketEU | WorldBitcoinExchange
            | LibertyBit | RMBTB | BitcashCZ | BitcoinHK | BidExtreme
            deriving (Show, Eq, Read, Bounded, Enum)

derivePersistField "Currency"
derivePersistField "Market"

marketFile :: Market -> Currency -> FilePath
marketFile LocalBitcoins c           = "data/localbtc" ++ show c ++ ".csv"
marketFile BTCmarkets c              = "data/btcmarkets" ++ show c ++ ".csv"
marketFile MtGox c                   = "data/mtgox" ++ show c ++ ".csv"
marketFile WeExchange c              = "data/weex" ++ show c ++ ".csv"
marketFile MercadoBitcoin c          = "data/mrcd" ++ show c ++ ".csv"
marketFile CanadianVirtualExchange c = "data/virtex" ++ show c ++ ".csv"
marketFile AsiaNexgen c              = "data/anxhk" ++ show c ++ ".csv"
marketFile BTCChina c                = "data/btcn" ++ show c ++ ".csv"
marketFile Bitcurex c                = "data/bitcurex" ++ show c ++ ".csv"
marketFile BitcoinDE c               = "data/btcde" ++ show c ++ ".csv"
marketFile BTCe c                    = "data/btce" ++ show c ++ ".csv"
marketFile CryptoTrade c             = "data/crytr" ++ show c ++ ".csv"
marketFile Intersango c              = "data/intrsng" ++ show c ++ ".csv"
marketFile Justcoin c                = "data/just" ++ show c ++ ".csv"
marketFile Ripple c                  = "data/ripple" ++ show c ++ ".csv"
marketFile RockTradingCompany c      = "data/rock" ++ show c ++ ".csv"
marketFile Vircurex c                = "data/vcx" ++ show c ++ ".csv"
marketFile Bit2C c                   = "data/bit2c" ++ show c ++ ".csv"
marketFile BitMe c                   = "data/bitme" ++ show c ++ ".csv"
marketFile BitNZ c                   = "data/bitnz" ++ show c ++ ".csv"
marketFile FYBSE c                   = "data/fybse" ++ show c ++ ".csv"
marketFile Kapiton c                 = "data/kptn" ++ show c ++ ".csv"
marketFile FYBSG c                   = "data/fybsg" ++ show c ++ ".csv"
marketFile VirWox c                  = "data/virwox" ++ show c ++ ".csv"
marketFile BitKonan c                = "data/bitkonan" ++ show c ++ ".csv"
marketFile BitStamp c                = "data/bitstamp" ++ show c ++ ".csv"
marketFile CampBX c                  = "data/cbx" ++ show c ++ ".csv"
marketFile ICBIT c                   = "data/icbit" ++ show c ++ ".csv"
marketFile BitX c                    = "data/bitx" ++ show c ++ ".csv"
marketFile BitMarketEU c             = "data/bitmarket" ++ show c ++ ".csv"
marketFile WorldBitcoinExchange c    = "data/wbx" ++ show c ++ ".csv"
marketFile LibertyBit c              = "data/lybit" ++ show c ++ ".csv"
marketFile RMBTB c                   = "data/rmbtb" ++ show c ++ ".csv"
marketFile BitcashCZ c               = "data/bitcash" ++ show c ++ ".csv"
marketFile BitcoinHK c               = "data/btchkex" ++ show c ++ ".csv"
marketFile BidExtreme c              = "data/bidxtrm" ++ show c ++ ".csv"

toCurrency :: String -> Currency
toCurrency = read . id
