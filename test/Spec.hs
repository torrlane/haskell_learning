module Main where
import           Hl.Csv.ModelSpec  (dividendTests, shareHoldingTests,
                                    transactionTests)
import           QuandlLookupSpec  (quandlLookupTests)
import           Test.Framework    (defaultMain)

main :: IO ()
main = defaultMain
        [
        shareHoldingTests,
        dividendTests,
        transactionTests,
        quandlLookupTests
        ]

