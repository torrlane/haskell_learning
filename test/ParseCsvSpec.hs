module ParseCsvSpec
    (
    parseCsvTests 
    )
where
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import Data.Time.Calendar                   (fromGregorian)
import Data.Map as Map                      (fromList)
import TestUtils                            (runParseRecordTest)
import Utils                                (toByteString, epoch)
import qualified Lib as A                   (Holding(..), dividends_paid_upto, parseHolding, createHoldings) 
import qualified Hl.Csv.Dividend as D       (Dividend(..))
import qualified Hl.Csv.Transaction as T    (Transaction(..))
import ParseCsv                             (getShareName)

parseCsvTests :: Test
parseCsvTests = testGroup "parseCsvTests" [
        testCase "testEmptyDividendCalculation" testEmptyDividendCalculation, 
        testCase "testDividendCalculation" testDividendCalculation,
        testCase "testParseHolding" testParseHolding,
        testCase "testCreateHoldings" testCreateHoldings
        ]
        
-- testParseShareHolding
{-
 - test dividends_paid_upto returns 0 when no dividends are supplied
 -}
testEmptyDividendCalculation :: Assertion
testEmptyDividendCalculation = assertEqual "" 0 $ A.dividends_paid_upto undefined [] []

testDividendCalculation :: Assertion
testDividendCalculation = 
    let purchaseDate = epoch
        dividendPaymentDate = epoch
        transaction = T.Transaction{T.actioned_on=purchaseDate,T.shares_bought=8,T.cost=0}
        dividend = D.Dividend{D.paid_on=dividendPaymentDate, D.amount=10} 
        expected = 80 
    in
    assertEqual "" expected $ A.dividends_paid_upto epoch [dividend] [transaction]



testParseHolding :: Assertion
testParseHolding = 
    let 
        expected = A.Holding{A.share="TSCO", A.transactions=[], A.dividends=[]} 
    in
    assertEqual "" expected $ A.parseHolding "Holding{share=\"TSCO\", transactions=[], dividends=[]}"




testCreateHoldings :: Assertion
testCreateHoldings =
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



