module Main where
import           Hl.Csv.ModelSpec (dividendTests, shareHoldingTests,
                                   transactionTests)
import           LibSpec          (libTests)
import           ParseCsvSpec     (parseCsvTests)
import           QuandlLookupSpec (quandlLookupTests)
import           Test.Framework   (defaultMain)

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

