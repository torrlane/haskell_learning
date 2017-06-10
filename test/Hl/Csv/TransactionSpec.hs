module Hl.Csv.TransactionSpec
    (
    transactionTests
    )
where
import           Data.Time.Calendar             (fromGregorian)
import qualified Hl.Csv.Transaction             as T (Transaction (..))
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)
import           TestUtils                      (runParseRecordTest)


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
