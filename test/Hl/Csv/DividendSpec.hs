module Hl.Csv.DividendSpec
    (
    dividendTests
    )
where
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import Data.Time.Calendar                   (fromGregorian)
import TestUtils                            (runParseRecordTest)
import qualified Hl.Csv.Dividend as D       (Dividend(Dividend, paid_on, amount))


dividendTests :: Test
dividendTests = testGroup "dividendTests" [
        testCase "test_parse_dividend" test_parse_dividend
        ]

test_parse_dividend :: Assertion
test_parse_dividend =
    let expected = D.Dividend{D.paid_on=fromGregorian 2016 12 07, D.amount=28.9}
        csvLine = ["07/12/2016","ST DIV","share name","n/a","80.00","23.12"]
    in
    assertEqual "" expected $ runParseRecordTest csvLine
