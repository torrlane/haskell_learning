module Main where
import           Hl.Csv.ModelSpec  (dividendTests, shareHoldingTests,
                                    transactionTests)
import           Hl.Csv.ModelSpec2 (shareHoldingTests2)
import           QuandlLookupSpec  (quandlLookupTests)
import           Test.Framework    (defaultMain)

main :: IO ()
main = defaultMain
        [
        shareHoldingTests,
        shareHoldingTests2,
        dividendTests,
        transactionTests,
        quandlLookupTests
        ]

