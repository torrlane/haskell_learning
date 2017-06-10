-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- The DuplicateRecordFields language extension allows records to use the same name for field labels. Without it, all the records in this module would need to have unique names for all their fields.
-}
module Lib
    (
        Valuation(Valuation, valued_on, price),
        Transaction(Transaction, actioned_on, shares_bought, cost),
        Holding(Holding, share, transactions, dividends),
        dividends_paid_upto,
        parseHolding,
        createHoldings,
    ) where
import Data.Time.Calendar               (Day(..))
import Data.Map             as M        (Map(..), mapWithKey, lookup, elems)
import Utils                            ((~=), delta)
import Hl.Csv.Dividend                  (Dividend(..))
import Hl.Csv.Transaction               (Transaction(..), number_held)


data Valuation = Valuation {valued_on :: Day, price :: Double} deriving (Show, Eq)


data Holding = Holding{ share :: String, transactions :: [Transaction], dividends :: [Dividend] } deriving (Read, Show, Eq)


{- Calculates the amount of dividends paid up to the specified date (inclusive) -}
dividends_paid_upto :: Day -> [Dividend] -> [Transaction] -> Double
dividends_paid_upto d ds ts = sum $ map (dividend_amount d) ds
    where
    dividend_amount day (Dividend paid_on amount)
        | paid_on > day = 0
        | otherwise = fromIntegral (number_held day ts) * amount


parseHolding :: String -> Holding
parseHolding s = read s :: Holding

{- Takes a Map of shareName to [Transaction] and a Map of shareName to [Dividend] and creates a [Holding] from the information in both Maps
 -}
createHoldings :: M.Map String [Transaction] -> M.Map String [Dividend] -> [Holding]
createHoldings tMap dMap = M.elems $ M.mapWithKey createHolding tMap
    where
    createHolding shareName ts = Holding{share=shareName, transactions=ts, dividends=lookupDividends }
        where lookupDividends = lift $ M.lookup shareName dMap
              lift Nothing = []
              lift (Just ds) = ds
