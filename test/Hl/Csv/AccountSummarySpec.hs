module Hl.Csv.AccountSummarySpec
    (
    shareHoldingTests,
    testParseShareHolding
    )
where

import Utils                                (toLazyByteString, parseDouble, stripDoubleQuotes)
import TestUtils                            (eitherParseRecordTest, runParseRecordTest)
import Data.Aeson.Types                     (parseMaybe)
import Data.Aeson                           (decode, parseJSON)
import QuandlLookup 
import Lib                                  (Valuation(..))
import Hl.Csv.AccountSummary as A           (ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice))
import Data.Time.Calendar                   (fromGregorian)
import Test.HUnit                           (assertEqual, Assertion)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework                       (Test, testGroup)

shareHoldingTests :: Test
shareHoldingTests = testGroup "ShareHoldingTests" [
    testCase "parseShareHolding" testParseShareHolding
    ]

testParseShareHolding :: Assertion
testParseShareHolding =
    let expected = A.ShareHolding{A.shareName="name", A.unitsHeld=4, A.sharePrice=1.5}
        --Stock,Units held,Price (pence),Value (£),Cost (£),Gain/loss (£),Gain/loss (%),Yield,Day change (pence),Day change (%),
        csvLine = ["\"name\"","\"4\"","\"1.50\"","\"1,981.44\"","\"2,012.36\"","\"-30.92\"","\"-1.54\"","\"1.02\"","\"4.50\"","\"0.44\""]
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest csvLine
