-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- The DuplicateRecordFields language extension allows records to use the same name for field labels. Without it, all the records in this module would need to have unique names for all their fields.
-}

{-
 - Lookup share data using the free Quandl rest api
 -}
module QuandlLookup
    (
    someFunc
    ) where
import           Control.Lens       ((^.))
import           Data.Aeson         (FromJSON (..), Value (..), withArray,
                                     withObject, (.:), (.=))
import           Data.Time.Calendar (Day (..))
import           Data.Time.Format   (defaultTimeLocale, parseTimeOrError)
import qualified Data.Vector        as V (toList)
import           Lib                (Valuation (..))
import           Network.Wreq       (Response (..), asJSON, get, responseBody)


instance FromJSON Valuation where
    parseJSON = withObject "valuation" $ \o -> do
        dataset_data <- o .: "dataset_data"
        data_array <- dataset_data .: "data"
        innerArray <- withArray "data_array" (parseJSON . head . V.toList) data_array
        dateString <- withArray "innerArray" (parseJSON . head . V.toList) innerArray
        price <- withArray "innerArray" (parseJSON . head . tail . V.toList) innerArray
        let date = parseDate dateString
        return Valuation{valued_on=date, price=price}


parseDate s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s :: Day

someFunc = do
    r <- asJSON =<< get quandl_url :: IO (Response Valuation)
    print $ show $ r ^. responseBody

quandl_url :: String
quandl_url = "https://www.quandl.com/api/v3/datasets/WIKI/FB/data.json?start_date=2016-10-01&end_date=2016-10-05"



