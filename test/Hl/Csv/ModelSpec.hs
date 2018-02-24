{-# LANGUAGE QuasiQuotes #-}
module Hl.Csv.ModelSpec
    (
    dividendTests,
    shareHoldingTests,
    testParseShareHolding,
    transactionTests
    )
where

import           Data.List.Split                (splitOn)
import           Data.Time.Calendar             (fromGregorian)
import           Hl.Csv.Model                   as M (Dividend (Dividend, amount, paidOn),
                                                      ShareHolding (ShareHolding, shareName, sharePrice, unitsHeld),
                                                      Transaction (..), dividendsPaidUpto, getShareName )
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)
import           TestUtils                      (eitherParseRecordTest,
                                                 runParseRecordTest)
import           Text.Heredoc                   (str)
import           Utils                          (epoch, dQuote)

shareHoldingTests :: Test
shareHoldingTests = testGroup "ShareHoldingTests" [
    testCase "parseShareHolding" testParseShareHolding,
    testCase "parseShareHolding2" testParseShareHolding2,
    testCase "test_getShareName_from_csvLine_1" test_getShareName_from_csvLine_1,
    testCase "test_getShareName_from_csvLine_2" test_getShareName_from_csvLine_2,
    testCase "test_getShareName_from_csvLine_3" test_getShareName_from_csvLine_3
    ]

testParseShareHolding2 :: Assertion
testParseShareHolding2 =
    let csvLine = ["Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1","192","1,032.00","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"]
        expected = M.ShareHolding{M.shareName="Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1", M.unitsHeld=192, M.sharePrice=1032}
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest $ map dQuote csvLine

testParseShareHolding :: Assertion
testParseShareHolding =
    let expected = M.ShareHolding{M.shareName="name", M.unitsHeld=4, M.sharePrice=1.5}
        --Stock,Units held,Price (pence),Value (),Cost (),Gain/loss (),Gain/loss (%),Yield,Day change (pence),Day change (%),
        csvLine = ["\"name\"","\"4\"","\"1.50\"","\"1,981.44\"","\"2,012.36\"","\"-30.92\"","\"-1.54\"","\"1.02\"","\"4.50\"","\"0.44\""]
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest csvLine

dividendTests :: Test
dividendTests = testGroup "dividendTests" [
        testCase "testParseDividend" testParseDividend,
        testCase "testParseBrwmDividend" testParseBrwmDividend
        ]

testParseDividend :: Assertion
testParseDividend =
    let expected = M.Dividend{M.paidOn=fromGregorian 2016 12 07, M.amount=28.9}
        csvLine = ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"]
    in
    assertEqual "" expected $ runParseRecordTest csvLine


testParseBrwmDividend :: Assertion
testParseBrwmDividend =
    let expected = M.Dividend{M.paidOn=fromGregorian 2016 12 07, M.amount=3}
        csvLine = ["07/12/2016","ST DIV","share name","n/a","826.00","24.78"]
    in
    assertEqual "" expected $ runParseRecordTest csvLine


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




parseCsvTests :: Test
parseCsvTests = testGroup "parseCsvTests" [
--        testCase "testEmptyDividendCalculation" testEmptyDividendCalculation,
--        testCase "testDividendCalculation" testDividendCalculation,
--        testCase "testParseHolding" testParseHolding,
--        testCase "testCreateHoldings" testCreateHoldings,
--        testCase "testParseShareHolding" testParseShareHolding
        ]
{-
testParseShareHolding :: Assertion
testParseShareHolding =
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
                          |Stock,Units held,Price (pence),Value (),Cost (),Gain/loss (),Gain/loss (%),Yield,Day change (pence),Day change (%),
                          |"Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1","192","1,032.00","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"
                          |]
        shareHolding = M.ShareHolding {M.shareName = "Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1", M.unitsHeld = 192.0, M.sharePrice = 1032.0}
    in
    assertEqual "" [shareHolding] $ parseShareHoldings csvContents
-}


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


-- Test that getShareName successfully extracts the share name from the csv line
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

