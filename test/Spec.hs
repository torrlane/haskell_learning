module Main where
import Hl.Csv.AccountSummarySpec            (shareHoldingTests)
import Hl.Csv.DividendSpec                  (dividendTests)
import Hl.Csv.TransactionSpec               (transactionTests)
import ParseCsvSpec                         (parseCsvTests)
import LibSpec                              (libTests)
import QuandlLookupSpec                     (quandlLookupTests)
import Test.Framework                       (defaultMain)

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

