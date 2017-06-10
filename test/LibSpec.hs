module LibSpec
    (
    libTests
    )
where
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import Data.Time.Calendar                   (fromGregorian)
import TestUtils                            (runParseRecordTest)
import ParseCsv                             (getShareName)

libTests :: Test
libTests = testGroup "libTests" [
        testCase "test_getShareName_from_csvLine_1" test_getShareName_from_csvLine_1,
        testCase "test_getShareName_from_csvLine_2" test_getShareName_from_csvLine_2,
        testCase "test_getShareName_from_csvLine_3" test_getShareName_from_csvLine_3
        ]


{-
 - Test that getShareName successfully extracts the share name from the csv line
 -}
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

