{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Hl.Csv.ModelSpec
    (
    dividendLogicTests,
    dividendTests,
    shareHoldingTests,
    testParseShareHoldingRecord,
    transactionTests,
    )
where

import           Data.List.Split                (splitOn)
import           Data.Text                      as T (append)
import           Data.Time.Calendar             (fromGregorian)
import           Hl.Csv.Model                   as M (Dividend (Dividend, amount, paidOn),
                                                      ShareHolding (ShareHolding, shareName, sharePrice, unitsHeld),
                                                      Transaction (..),
                                                      dividendsPaidUpto,
                                                      getShareName, parseShareHoldings)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)
import           TestUtils                      (eitherParseRecordTest,
                                                 runParseRecordTest)
import           Text.Heredoc                   (str)
import           Utils                          (dQuote, epoch)

shareHoldingTests :: Test
shareHoldingTests = testGroup "ShareHoldingTests" [
    testCase "testParseShareHoldingRecord" testParseShareHoldingRecord
      , testCase "parseShareHolding2" testParseShareHolding2
      , testCase "test_getShareName_from_csvLine" test_getShareName_from_csvLine
      , testCase "testParseShareHolding3" testParseShareHolding3
    ]

testParseShareHolding2 :: Assertion
testParseShareHolding2 =
    let csvLine = ["Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1","192","1,032.00","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"]
        expected = M.ShareHolding{M.shareName="Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1", M.unitsHeld=192, M.sharePrice=1032}
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest $ map dQuote csvLine

-- Test that getShareName successfully extracts the share name from the csv line
test_getShareName_from_csvLine :: Assertion
test_getShareName_from_csvLine =
    let expected1 = "Henderson International Income Trust plc"
        expected2 = "Royal Dutch Shell Plc A Shares"
        expected3 = "Biotech Growth Trust (The)"
        csvLine1 = T.append "Income history for:, " $ T.append expected1 ", Ord GBP0.01 , , ,"
        csvLine2 = T.append "Income history for:, " $ T.append expected2 ", EUR0.07 , , ,"
        csvLine3 = T.append "Security movements for:, " $ T.append expected3 ", Ordinary 25p , , ,"
    in
    do
      assertEqual "" (Just expected1) $ getShareName csvLine1
      assertEqual "" (Just expected2) $ getShareName csvLine2
      assertEqual "" (Just expected3) $ getShareName csvLine3

testParseShareHoldingRecord :: Assertion
testParseShareHoldingRecord =
    let expected = M.ShareHolding{M.shareName="name", M.unitsHeld=4, M.sharePrice=1.5}
        --Stock,Units held,Price (pence),Value (),Cost (),Gain/loss (),Gain/loss (%),Yield,Day change (pence),Day change (%),
        csvLine = ["name","4","1.50","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"]
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest $ map dQuote csvLine

testParseShareHolding3 :: Assertion
testParseShareHolding3 =
    let csvContents = [str|HL Vantage SIPP, , , ,
                          |Client Name:,Mr JoeBlogs, , ,
                          |Client Number:, 1234678910, , ,
                          |Spreadsheet created at,14-05-2017 01:38, , ,
                          |
                          |Stock value:,"12,345.67", , ,
                          |Total cash:,"12,345.67", , ,
                          |Amount available to invest:,"12,345.67", , ,
                          |Total value:,"12,345.67", , ,
                          |
                          |Stock,Units held,Price (pence),Value (£),Cost (£),Gain/loss (£),Gain/loss (%),Yield,Day gain/loss (£),Day gain/loss (%),
                          |"Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1","192","1,032.00","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"
                          |]
        shareHolding = M.ShareHolding {M.shareName = "Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1", M.unitsHeld = 192.0, M.sharePrice = 1032.0}
    in
    assertEqual "" [shareHolding] $ parseShareHoldings csvContents



dividendTests :: Test
dividendTests = testGroup "dividendTests" [
    testCase "testParseDividend" testParseDividend
    ]

testParseDividend :: Assertion
testParseDividend =
    let expected1 = M.Dividend{M.paidOn=fromGregorian 2016 12 07, M.amount=28.9}
        expected2 = M.Dividend{M.paidOn=fromGregorian 2016 12 07, M.amount=3}
        csvLine1 = ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"]
        csvLine2 = ["07/12/2016","ST DIV","share name","n/a","826.00","24.78"]
    in
    do
    assertEqual "" expected1 $ runParseRecordTest csvLine1
    assertEqual "" expected2 $ runParseRecordTest csvLine2




transactionTests :: Test
transactionTests = testGroup "transactionTests" [
        testCase "test_parse_transaction" test_parse_transaction
        ]

test_parse_transaction :: Assertion
test_parse_transaction =
    let expectedTransactionDate = fromGregorian 2016 12 07
        csvTransactionDate = "07/12/2016"
        expected = M.Transaction {M.actionedOn=expectedTransactionDate, M.sharesBought=1, M.cost=11}
    in
    assertEqual "" expected $ runParseRecordTest [csvTransactionDate, "_", "_", "_", "1.00", "11"]




dividendLogicTests :: Test
dividendLogicTests = testGroup "parseCsvTests" [
        testCase "testEmptyDividendCalculation" testEmptyDividendCalculation,
        testCase "testDividendCalculation" testDividendCalculation
        ]

{-
 - test dividendsPaidUpto returns 0 when no dividends are supplied
 -}
testEmptyDividendCalculation :: Assertion
testEmptyDividendCalculation = assertEqual "" 0 $ M.dividendsPaidUpto undefined [] []

testDividendCalculation :: Assertion
testDividendCalculation =
    let purchaseDate = epoch
        dividendPaymentDate = epoch
        transaction = M.Transaction{M.actionedOn=purchaseDate,M.sharesBought=8,M.cost=0}
        dividend = M.Dividend{M.paidOn=dividendPaymentDate, M.amount=10}
        expected = 80
    in
    assertEqual "" expected $ M.dividendsPaidUpto epoch [dividend] [transaction]

