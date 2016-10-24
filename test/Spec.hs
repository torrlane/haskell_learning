{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as L
import Data.Aeson.Types (parseMaybe)
import Data.Aeson (decode)
import Data.Time.Calendar (fromGregorian)
import Lib (Valuation(..), parseJSON)

json1 = "{\n\
        \   \"dataset_data\":\n\
        \   {\n\
        \   \"limit\":null,\"transform\":null,\"column_index\":null,\n\
        \   \"column_names\":[\"Date\",\"Open\",\"High\",\"Low\",\"Close\",\"Volume\",\"Ex-Dividend\",\"Split Ratio\",\"Adj. Open\",\"Adj. High\",\"Adj. Low\",\"Adj. Close\",\"Adj. Volume\"],\n\
        \   \"start_date\":\"2016-10-01\",\"end_date\":\"2016-10-05\",\"frequency\":\"daily\",\n\
        \   \"data\":[\n\
        \       [\"2016-10-05\",128.25,128.8,127.83,128.47,12344121.0,0.0,1.0,128.25,128.8,127.83,128.47,12344121.0],\n\
        \       [\"2016-10-04\",129.17,129.2765,127.5499,128.18,14250845.0,0.0,1.0,129.17,129.2765,127.5499,128.18,14250845.0],\n\
        \       [\"2016-10-03\",128.38,129.09,127.8,128.77,13071915.0,0.0,1.0,128.38,129.09,127.8,128.77,13071915.0]],\n\
        \       \"collapse\":null,\"order\":null\n\
        \   }\n\
        \}"

test1 = assertEqual "valuations equal? " (Just Valuation{valued_on=now, price=128.25}) $ parse json1

json2 = "{\n\
        \   \"dataset_data\":\n\
        \   {\n\
        \   \"data\":[\n\
        \   [\"2016-10-05\", 5]\n\
        \   ]\n\
        \   }\n\
        \}"

test2 = assertEqual "dates equal?" (Just now) $ valued_on <$> (parse json2)

now = fromGregorian 2016 10 05 


parse :: String -> Maybe Valuation
parse s = parseMaybe parseJSON =<< decode (T.encodeUtf8 (L.pack s))



main :: IO ()
main = defaultMainWithOpts
        [testCase "test1" test1, testCase "test2" test2] mempty
