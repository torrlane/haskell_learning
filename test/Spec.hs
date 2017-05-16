{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Utils                                (toByteString, epoch)
import TestUtils                            (runParseRecordTest)
import QuandlLookupSpec                     (test1, test2)
import Hl.Csv.AccountSummarySpec            (testParseShareHolding)
import Test.Framework                       (defaultMain, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import Data.Time.Calendar                   (fromGregorian)
import Data.Map as Map                      (fromList)
import qualified Lib as A                   (Holding(..), dividends_paid_upto, parseHolding, createHoldings) 
import qualified Hl.Csv.Dividend as D       (Dividend(..))
import qualified Hl.Csv.Transaction as T    (Transaction(..))
import ParseCsv


{-
 - test dividends_paid_upto returns 0 when no dividends are supplied
 -}
test_empty_dividend_calculation :: Assertion
test_empty_dividend_calculation = assertEqual "" 0 $ A.dividends_paid_upto undefined [] []

test_dividend_calculation :: Assertion
test_dividend_calculation = 
    let purchaseDate = epoch
        dividendPaymentDate = epoch
        transaction = T.Transaction{T.actioned_on=purchaseDate,T.shares_bought=8,T.cost=0}
        dividend = D.Dividend{D.paid_on=dividendPaymentDate, D.amount=10} 
        expected = 80 
    in
    assertEqual "" expected $ A.dividends_paid_upto epoch [dividend] [transaction]

test_parseHolding :: Assertion
test_parseHolding = 
    let 
        expected = A.Holding{A.share="TSCO", A.transactions=[], A.dividends=[]} 
    in
    assertEqual "" expected $ A.parseHolding "Holding{share=\"TSCO\", transactions=[], dividends=[]}"

{-
 - Test that getShareName successfully extracts the share name from the csv line
 -}
test_getShareName_from_csvLine_1 :: Assertion
test_getShareName_from_csvLine_1 = 
    let expected = "Henderson International Income Trust plc" 
        csvLine = "Income history for:, " ++ expected ++ ", Ord GBP0.01 , , ," 
    in
    assertEqual "" (Just expected) $ getShareName csvLine

test_getShareName_from_csvLine_2 :: Assertion
test_getShareName_from_csvLine_2 = 
    let expected = "Royal Dutch Shell Plc A Shares"
        csvLine = "Income history for:, " ++ expected ++ ", EUR0.07 , , ," 
    in
    assertEqual "" (Just expected) $ getShareName csvLine

test_getShareName_from_csvLine_3 :: Assertion
test_getShareName_from_csvLine_3 = 
    let expected = "Biotech Growth Trust (The)"
        csvLine = "Security movements for:, " ++ expected ++ ", Ordinary 25p , , ," 
    in
    assertEqual "" (Just expected) $ getShareName csvLine

test_parse_transaction :: Assertion
test_parse_transaction = 
    let expectedTransactionDate = fromGregorian 2016 12 07
        csvTransactionDate = "07/12/2016"
        expected = T.Transaction {T.actioned_on=expectedTransactionDate, T.shares_bought=1, T.cost=11} 
    in
    assertEqual "" expected $ runParseRecordTest [csvTransactionDate, "_", "_", "_", "1.00", "11"] 

test_parse_dividend :: Assertion
test_parse_dividend = 
    let expected = D.Dividend{D.paid_on=fromGregorian 2016 12 07, D.amount=28.9}
        csvLine = ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"] 
    in
    assertEqual "" expected $ runParseRecordTest csvLine



test_create_holdings :: Assertion
test_create_holdings =
    -- transaction with no dividends
    let transactionDate1 = fromGregorian 2016 12 07
        transaction1 = T.Transaction{T.actioned_on=transactionDate1, T.shares_bought=1, T.cost=11} 
    -- holding with two transactions and 2 dividends
        transactionDate2_1 = fromGregorian 2016 11 07
        transactionDate2_2 = fromGregorian 2017 11 07
        transaction2_1 = T.Transaction{T.actioned_on=transactionDate2_1, T.shares_bought=1, T.cost=11} 
        transaction2_2 = T.Transaction{T.actioned_on=transactionDate2_2, T.shares_bought=4, T.cost=14} 
        dividend2_1 = D.Dividend{D.paid_on=fromGregorian 2017 01 01, D.amount=28.9}
        dividend2_2 = D.Dividend{D.paid_on=fromGregorian 2017 02 01, D.amount=29.1}
        holding1 = A.Holding{A.share="share1", A.transactions=[transaction1], A.dividends=[]}
        holding2 = A.Holding{A.share="share2", A.transactions=[transaction2_1, transaction2_2], A.dividends=[dividend2_1, dividend2_2]}
    -- dividend with no corresponding transaction
        dividend3 = D.Dividend{D.paid_on=fromGregorian 2017 04 01, D.amount=1.9}
        transactionsMap = fromList [("share1", [transaction1]), ("share2", [transaction2_1, transaction2_2])]
        dividendsMap = fromList [("share1", []), ("share2", [dividend2_1, dividend2_2]), ("share3", [dividend3])]
    in
    assertEqual "holdings fail" [holding1, holding2] $ A.createHoldings transactionsMap dividendsMap



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
        testCase "test_parse_dividend" test_parse_dividend,
        testCase "test_create_holdings" test_create_holdings,
        testCase "testParseShareHolding" testParseShareHolding

        ]
