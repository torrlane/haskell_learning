module Hl.Csv.DividendSpec
    (
    dividendTests
    )
where
import           Data.Time.Calendar             (fromGregorian)
import qualified Hl.Csv.Dividend                as D (Dividend (Dividend, amount, paidOn))
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)
import           TestUtils                      (runParseRecordTest)


dividendTests :: Test
dividendTests = testGroup "dividendTests" [
        testCase "testParseDividend" testParseDividend
        ]

testParseDividend :: Assertion
testParseDividend =
    let expected = D.Dividend{D.paidOn=fromGregorian 2016 12 07, D.amount=28.9}
        csvLine = ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"]
    in
    assertEqual "" expected $ runParseRecordTest csvLine
