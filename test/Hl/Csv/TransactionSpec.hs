module Hl.Csv.TransactionSpec
    (
    transactionTests
    )
where
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import Data.Time.Calendar                   (fromGregorian)
import TestUtils                            (runParseRecordTest)
import qualified Hl.Csv.Transaction as T    (Transaction(..))


transactionTests :: Test
transactionTests = testGroup "transactionTests" [
        testCase "test_parse_transaction" test_parse_transaction
        ]

test_parse_transaction :: Assertion
test_parse_transaction =
    let expectedTransactionDate = fromGregorian 2016 12 07
        csvTransactionDate = "07/12/2016"
        expected = T.Transaction {T.actioned_on=expectedTransactionDate, T.shares_bought=1, T.cost=11}
    in
    assertEqual "" expected $ runParseRecordTest [csvTransactionDate, "_", "_", "_", "1.00", "11"]
