-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- The DuplicateRecordFields language extension allows records to use the same name for field labels. Without it, all the records in this module would need to have unique names for all their fields.
-}
module Lib
    (
        Valuation(Valuation, valuedOn, price),
        Transaction(Transaction, actionedOn, sharesBought, cost),
        dividendsPaidUpto,
    ) where
import           Data.Map           as M (Map (..), elems, lookup, mapWithKey)
import           Data.Time.Calendar (Day (..))
import           Hl.Csv.Model       (Dividend (..), Transaction (..),
                                     numberHeld)
import           Utils              (delta, (~=))


data Valuation = Valuation {valuedOn :: Day, price :: Double} deriving (Show, Eq)



{- Calculates the amount of dividends paid up to the specified date (inclusive) -}
dividendsPaidUpto :: Day -> [Dividend] -> [Transaction] -> Double
dividendsPaidUpto d ds ts = sum $ map (dividend_amount d) ds
    where
    dividend_amount day (Dividend paid_on amount)
        | paid_on > day = 0
        | otherwise = fromIntegral (numberHeld day ts) * amount

