-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- The DuplicateRecordFields language extension allows records to use the same name for field labels. Without it, all the records in this module would need to have unique names for all their fields.
-}
module Lib
    ( 
    someFunc
    ) where
import Data.Time.Calendar                   (Day(..))
import Control.Lens                         ( (^.) )
import Data.Time.Format                     (parseTimeOrError, defaultTimeLocale)
import qualified Data.Vector as V           (toList)
import Network.Wreq                         (Response(..), asJSON, get, responseBody)
import Data.Aeson                           (Value(..), FromJSON(..), (.:), (.=), withObject, withArray)
import AltLib                               (Valuation(..))


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



{-
purchase number of shares, cost, date
valuation share, value, date
income date paid, amount.
-}
