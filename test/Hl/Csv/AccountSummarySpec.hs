{-# LANGUAGE  QuasiQuotes #-}
module Hl.Csv.AccountSummarySpec
    (
    shareHoldingTests,
    testParseShareHolding
    )
where

import Data.List.Split                      (splitOn)
import Hl.Csv.AccountSummary as A           (ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import TestUtils                            (eitherParseRecordTest)
import Text.Heredoc                         (str)

shareHoldingTests :: Test
shareHoldingTests = testGroup "ShareHoldingTests" [
    testCase "parseShareHolding" testParseShareHolding,
    testCase "parseShareHolding2" testParseShareHolding2
    ]

testParseShareHolding2 :: Assertion
testParseShareHolding2 =
    let csvLine = [str|"Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1","192","1,032.00","1,981.44","2,012.36","-30.92","-1.54","1.02","4.50","0.44"|]
        expected = A.ShareHolding{A.shareName="Aberdeen Asian Smaller Companies Investment Trust Ordinary 25p *1", A.unitsHeld=192, A.sharePrice=1.0}
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest $ splitOn "," csvLine

testParseShareHolding :: Assertion
testParseShareHolding =
    let expected = A.ShareHolding{A.shareName="name", A.unitsHeld=4, A.sharePrice=1.5}
        --Stock,Units held,Price (pence),Value (œ),Cost (œ),Gain/loss (œ),Gain/loss (%),Yield,Day change (pence),Day change (%),
        csvLine = ["\"name\"","\"4\"","\"1.50\"","\"1,981.44\"","\"2,012.36\"","\"-30.92\"","\"-1.54\"","\"1.02\"","\"4.50\"","\"0.44\""]
    in
    assertEqual "" (Right expected) $ eitherParseRecordTest csvLine
