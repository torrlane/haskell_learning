
module ParseCsvSpec
    (
    parseCsvTests
    )
where
import           Data.Map                       as Map (fromList)
import           Data.Time.Calendar             (fromGregorian)
import qualified Hl.Csv.Model                   as M (Dividend (..), ShareHolding (ShareHolding, shareName, sharePrice, unitsHeld),
                                                      Transaction (..))
import qualified Lib                            as A (Holding (..),
                                                      createHoldings,
                                                      dividendsPaidUpto,
                                                      parseHolding)
import           ParseCsv                       (getShareName)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)
import           TestUtils                      (runParseRecordTest)
import           Text.Heredoc                   (str)
import           Utils                          (epoch, toByteString)

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
testEmptyDividendCalculation = assertEqual "" 0 $ A.dividendsPaidUpto undefined [] []

testDividendCalculation :: Assertion
testDividendCalculation =
    let purchaseDate = epoch
        dividendPaymentDate = epoch
        transaction = M.Transaction{M.actionedOn=purchaseDate,M.sharesBought=8,M.cost=0}
        dividend = M.Dividend{M.paidOn=dividendPaymentDate, M.amount=10}
        expected = 80
    in
    assertEqual "" expected $ A.dividendsPaidUpto epoch [dividend] [transaction]



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
        transaction1 = M.Transaction{M.actionedOn=transactionDate1, M.sharesBought=1, M.cost=11}
    -- holding with two transactions and 2 dividends
        transactionDate2_1 = fromGregorian 2016 11 07
        transactionDate2_2 = fromGregorian 2017 11 07
        transaction2_1 = M.Transaction{M.actionedOn=transactionDate2_1, M.sharesBought=1, M.cost=11}
        transaction2_2 = M.Transaction{M.actionedOn=transactionDate2_2, M.sharesBought=4, M.cost=14}
        dividend2_1 = M.Dividend{M.paidOn=fromGregorian 2017 01 01, M.amount=28.9}
        dividend2_2 = M.Dividend{M.paidOn=fromGregorian 2017 02 01, M.amount=29.1}
        holding1 = A.Holding{A.share="share1", A.transactions=[transaction1], A.dividends=[]}
        holding2 = A.Holding{A.share="share2", A.transactions=[transaction2_1, transaction2_2], A.dividends=[dividend2_1, dividend2_2]}
    -- dividend with no corresponding transaction
        dividend3 = M.Dividend{M.paidOn=fromGregorian 2017 04 01, M.amount=1.9}
        transactionsMap = fromList [("share1", [transaction1]), ("share2", [transaction2_1, transaction2_2])]
        dividendsMap = fromList [("share1", []), ("share2", [dividend2_1, dividend2_2]), ("share3", [dividend3])]
    in
    assertEqual "holdings fail" [holding1, holding2] $ A.createHoldings transactionsMap dividendsMap



