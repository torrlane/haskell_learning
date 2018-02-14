
module ParseCsvSpec
    (
    parseCsvTests
    )
where
import           Data.Map                       as Map (fromList)
import           Data.Time.Calendar             (fromGregorian)
import qualified Hl.Csv.Model                   as M (Dividend (..), ShareHolding (ShareHolding, shareName, sharePrice, unitsHeld),
                                                      Transaction (..))
import qualified Lib                            as A (dividendsPaidUpto)
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

