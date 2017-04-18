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


{-
 - test dividends_paid_upto returns 0 when no dividends are supplied
 -}
test_empty_dividend_calculation :: Assertion
test_empty_dividend_calculation = assertEqual "" 0 $ A.dividends_paid_upto undefined [] []

test_dividend_calculation = 
    let purchaseDate = epoch
        dividendPaymentDate = epoch
        transaction = A.Transaction{A.transaction_date=purchaseDate,A.shares_bought=8,A.cost=0}
        dividend = A.Dividend{A.paid_on=dividendPaymentDate, A.amount=10} 
        expected = 80 in
    assertEqual "" expected $ A.dividends_paid_upto epoch [dividend] [transaction]

test_parseHolding = 
    let expected = A.Holding{A.share="TSCO", A.transactions=[], A.dividends=[]} in
    assertEqual "" expected $ A.parseHolding "Holding{share=\"TSCO\", transactions=[], dividends=[]}"

{-
 - Test that getShareName successfully extracts the share name from the csv line
 -}
test_getShareName_from_csvLine_1 = 
    let expected = "Henderson International Income Trust plc" 
        csvLine = "Income history for:, " ++ expected ++ ", Ord GBP0.01 , , ," in
    assertEqual "" Just expected $ getShareName csvLine

test_getShareName_from_csvLine_2 = 
    let expected = "Royal Dutch Shell Plc A Shares"
        csvLine = "Income history for:, " ++ expected ++ ", EUR0.07 , , ," in
    assertEqual "" Just expected $ getShareName csvLine

test_getShareName_from_csvLine_3 = 
    let expected = "Biotech Growth Trust (The)"
        csvLine = "Security movements for:, " ++ expected ++ ", Ordinary 25p , , ," in
    assertEqual "" Just expected $ getShareName csvLine

test_parse_transaction = 
    let expectedTransactionDate = fromGregorian 2016 12 07
        csvTransactionDate = "07/12/2016"
        expected = A.Transaction {A.transaction_date=expectedTransactionDate, A.shares_bought=1, A.cost=11} in
    assertEqual "" expected $ runParseRecordTest [csvTransactionDate, "_", "_", "_", "1.00", "11"] 

test_parse_dividend = 
    let expected = A.Dividend{A.paid_on=fromGregorian 2016 12 07, A.amount=28.9}
        csvLine = ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"] in
    assertEqual "" expected $ runParseRecordTest csvLine

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
        testCase "test_empty_dividend_calculation" test_empty_dividend_calculation, 
        testCase "test_dividend_calculation" test_dividend_calculation,
        testCase "test_parseHolding" test_parseHolding,
        testCase "test_getShareName_from_csvLine_1" test_getShareName_from_csvLine_1,
        testCase "test_getShareName_from_csvLine_2" test_getShareName_from_csvLine_2,
        testCase "test_getShareName_from_csvLine_3" test_getShareName_from_csvLine_3,
        testCase "test_parse_transaction" test_parse_transaction,
        testCase "test_parse_dividend" test_parse_dividend
        ]
