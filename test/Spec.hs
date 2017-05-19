{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Utils                                (toByteString, epoch)
import TestUtils                            (runParseRecordTest)
import Hl.Csv.AccountSummarySpec            (testParseShareHolding, shareHoldingTests)
import Hl.Csv.DividendSpec                  (dividendTests)
import Hl.Csv.TransactionSpec               (transactionTests)
import ParseCsvSpec                         (parseCsvTests)
import LibSpec                              (libTests)
import QuandlLookupSpec                     (quandlLookupTests)
import Test.Framework                       (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual, Assertion)
import Data.Time.Calendar                   (fromGregorian)
import Data.Map as Map                      (fromList)
import ParseCsv

main :: IO ()
main = defaultMain
        [
        shareHoldingTests,
        dividendTests,
        transactionTests,
        parseCsvTests,
        libTests,
        quandlLookupTests
        ]

