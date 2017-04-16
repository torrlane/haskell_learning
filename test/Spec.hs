{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Utils (toByteString, epoch)
import AesonSpec (test1, test2)
import Test.Framework (defaultMainWithOpts, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import Data.Time.Calendar (fromGregorian)
import Data.Csv (Parser(..), FromRecord(..), record, parseRecord, runParser)
import Data.Either.Combinators (fromRight, fromRight')
import qualified AltLib as A (Dividend(..), Transaction(..), Holding(..), dividends_paid_upto, parseHolding) 
import ParseDividends


test_empty_dividend_calculation :: Assertion
test_empty_dividend_calculation = assertEqual "fail dividend" 0 $ A.dividends_paid_upto epoch [] []

test_dividend_calculation = assertEqual "fail dividend" 80 $ A.dividends_paid_upto epoch [A.Dividend{A.paid_on=epoch, A.amount=10}] [A.Transaction{A.transaction_date=epoch,A.shares_bought=8,A.cost=8}]


test_parseHolding = assertEqual "failed to parseLine " A.Holding{A.share="TSCO", A.transactions=[], A.dividends=[]} $ A.parseHolding "Holding{share=\"TSCO\", transactions=[], dividends=[]}"

test_get_share1 = assertEqual "unlucky!" (Just "Henderson International Income Trust plc") $ getShareName "Income history for:, Henderson International Income Trust plc, Ord GBP0.01 , , ,"

test_get_share2 = assertEqual "unlucky!" (Just "Royal Dutch Shell Plc A Shares") $ getShareName "Income history for:, Royal Dutch Shell Plc A Shares, EUR0.07 , , ,"

test_get_share3 = assertEqual "" (Just "Biotech Growth Trust (The)") $ getShareName "Security movements for:, Biotech Growth Trust (The), Ordinary 25p , , ,"

test_parse_transaction = assertEqual "" A.Transaction {A.transaction_date=fromGregorian 2016 12 07, A.shares_bought=1, A.cost=11} $ runParseRecordTest ["07/12/2016", "_", "_", "_", "1.00", "11"] 

test_parse_dividend = assertEqual "" A.Dividend{A.paid_on=fromGregorian 2016 12 07, A.amount=28.9} $ runParseRecordTest ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"]

{-
 - Take a "csv" as input and parse it into a type using the Data.Csv.parseRecord method, which will, in turn, use the appropriate fromRecord function for the desired type.
 - We assume success and return the right hand side of the Either.
 -}
runParseRecordTest :: FromRecord a => [String] -> a
runParseRecordTest xs = fromRight' . runParser . parseRecord . record $ fmap toByteString xs




main :: IO ()
main = defaultMain
        [testCase "test1" test1, 
        testCase "test2" test2, 
        testCase "test4" test_empty_dividend_calculation, 
        testCase "test5" test_dividend_calculation,
        testCase "test6" test_parseHolding,
        testCase "get_share1" test_get_share1,
        testCase "get_share2" test_get_share2,
        testCase "get_share3" test_get_share3,
        testCase "test_parse_transaction" test_parse_transaction,
        testCase "test_parse_dividend" test_parse_dividend
        ]
