{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- The DuplicateRecordFields language extension allows records to use the same name for field labels. Without it, all the records in this module would need to have unique names for all their fields.
-}
module Lib
    ( someFunc,
        Valuation(..),parseJSON
    ) where
import Data.Time.Calendar
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Control.Lens
import qualified Data.Vector as V
import Network.Wreq
import Data.Aeson (Value(..), FromJSON(..), (.:), (.=), withObject, withArray)

data Valuation = Valuation {valued_on :: Day, price :: Double} deriving (Show, Eq)

instance FromJSON Valuation where
    parseJSON = withObject "valuation" $ \o -> do
        dataset_data <- o .: "dataset_data"
        data_array <- dataset_data .: "data"
        innerArray <- withArray "data_array" (\arr -> parseJSON (head (V.toList arr))) $ data_array
        dateString <- withArray "innerArray" (\arr -> parseJSON (head (V.toList arr))) $ innerArray
        price <- withArray "innerArray" (\arr -> parseJSON (head (tail (V.toList arr)))) $ innerArray
        let date = parseDate $ dateString 
        return Valuation{valued_on=date, price=price}
        

parseDate s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s :: Day

someFunc = do
    r <- asJSON =<< get quandl_url :: IO (Response Valuation)
    print $ show $ r ^. responseBody 
    
quandl_url :: String
quandl_url = "https://www.quandl.com/api/v3/datasets/WIKI/FB/data.json?start_date=2016-10-01&end_date=2016-10-05"


data Share = TSCO | HSBA 

data Transaction = Purchase {num :: Int, cost :: Double, purchased_on :: Day} | Sell {num :: Int, value :: Double, sold_on :: Day}

data Dividend = Dividend { paid_on :: Day, amount :: Double}

{- Gives the amount of dividends paid up to the specified date (inclusive) -}
dividends_paid_upto :: Day -> [Dividend] -> [Transaction] -> Double
dividends_paid_upto d [] _ = 0
dividends_paid_upto day (d:ds) ts = (dividend_amount day d) + dividends_paid_upto day ds ts
    where 
    dividend_amount day (Dividend paid_on amount)
        | paid_on >= day = 0
        | otherwise = fromIntegral (number_held ts day) * amount

{- The value of the holding on the specified date -}
value_on :: [Transaction] -> Valuation -> Double
value_on [] _ = 0
value_on xs (Valuation d p) = fromIntegral (number_held xs d) * p

{- the number of shares held on the specified date (inclusive)-}
number_held :: [Transaction] -> Day -> Int
number_held [] _ = 0
number_held (t:ts) day =  calculateChangToHolding t + number_held ts day 
    where 
    calculateChangToHolding (Purchase number _ purchased_date )
        | purchased_date >= day = 0
        | otherwise = number
    calculateChangToHolding (Sell number _ sold_date)
        | sold_date >= day = 0
        | otherwise = -number

{-
purchase number of shares, cost, date
valuation share, value, date
income date paid, amount.

-}
