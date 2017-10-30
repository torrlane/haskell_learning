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
                                                      Transaction (..))
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)
import           TestUtils                      (eitherParseRecordTest,
                                                 runParseRecordTest)
import           Text.Heredoc                   (str)

shareHoldingTests :: Test
shareHoldingTests = testGroup "ShareHoldingTests" [
    testCase "parseShareHolding" testParseShareHolding,
    testCase "parseShareHolding2" testParseShareHolding2
    ]

testParseShareHolding2 :: Assertion
testParseShareHolding2 =
    let csvLine = [str|"Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1","192","1,032.00","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"|]
        expected = M.ShareHolding{M.shareName="Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1", M.unitsHeld=192, M.sharePrice=1.0}
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest $ splitOn "," csvLine

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
