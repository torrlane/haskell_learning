{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Test.Framework (defaultMainWithOpts, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8)
import qualified Data.Text.Lazy as L (pack)
import Data.Aeson.Types (parseMaybe)
import Data.Aeson (decode)
import Data.Time.Calendar (fromGregorian)
import Lib (Valuation(..), parseJSON)
import qualified AltLib as A (Dividend(..), Transaction(..), Holding(..), dividends_paid_upto, parseHolding) 

{- multiline strings -}
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


{- 
 - L.pack convert the String into a Text 
 - E.encodeUtf8 converts the Text into a utf8 encoded ByteString
 - decode takes a ByteString and returns Maybe Value 
 - parseJSON takes a Value and returns a Parser Valuation (by type inference from the signature)
 - parseMaybe takes a function from a -> Parser b, an instance of a and returns Maybe b
 -
 - decode :: FromJSON a => ByteString -> Maybe Value(a)
 - parseMaybe :: (Value(a) -> Parser b) -> Value(a) -> Maybe b
 - parseJSON :: Value -> Parser a
 -}
parse :: String -> Maybe Valuation
parse s = parseMaybe parseJSON =<< decode (E.encodeUtf8 (L.pack s))


test_empty_dividend_calculation = assertEqual "fail dividend" 0 $ A.dividends_paid_upto now [] []

test_dividend_calculation = assertEqual "fail dividend" 80 $ A.dividends_paid_upto now [A.Dividend{A.paid_on=now, A.amount=10}] [A.Transaction{A.transaction_date=now,A.shares_bought=8,A.cost=8}]


test_parseHolding = assertEqual "failed to parseLine " A.Holding{A.share="TSCO", A.transactions=[], A.dividends=[]} $ A.parseHolding "Holding{share=\"TSCO\", transactions=[], dividends=[]}"

main :: IO ()
main = defaultMain
        [testCase "test1" test1, 
        testCase "test2" test2, 
        testCase "test4" test_empty_dividend_calculation, 
        testCase "test5" test_dividend_calculation,
        testCase "test6" test_parseHolding]
