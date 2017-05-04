{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{- The DuplicateRecordFields language extension allows records to use the same name for field labels. Without it, all the records in this module would need to have unique names for all their fields.
-}
module AltLib
    ( 
        Dividend(..),
        Transaction(..),
        Holding(..),
        dividends_paid_upto,
        parseHolding,
        createHoldings
    ) where
import Data.Time.Calendar (Day(..))
import Data.Map as M (Map(..), mapWithKey, lookup, elems)


{- 
 - cost is the total cost of the Transaction, not the individual cost per unit
 - -}
data Transaction = Transaction{ actioned_on :: Day, shares_bought:: Int, cost :: Double } deriving (Read, Show, Eq)

data Dividend = Dividend{ paid_on :: Day, amount :: Double } deriving (Read, Show)

-- equals doesn't work well for Doubles. Make Dividend an instance of Eq and use a small error when comparing the Dividend amounts
instance Eq Dividend where
    a == b = (paid_on a) == (paid_on b) && (amount a) ~= (amount b)

data Holding = Holding{ share :: String, transactions :: [Transaction], dividends :: [Dividend] } deriving (Read, Show, Eq)

{- 'roughly equal' evaluates to true if the two input values are roughly equivalent -}
(~=) :: Double -> Double -> Bool
x ~= y = (x + delta) > y && (x - delta) < y

delta :: Double
delta = 0.000001

{- Calculates the amount of dividends paid up to the specified date (inclusive) -}
dividends_paid_upto :: Day -> [Dividend] -> [Transaction] -> Double
dividends_paid_upto d ds ts = sum $ map (dividend_amount d) ds
    where 
    dividend_amount day (Dividend paid_on amount)
        | paid_on > day = 0
        | otherwise = fromIntegral (number_held day ts) * amount

{- the number of shares held on the specified date (inclusive)-}
number_held :: Day -> [Transaction] -> Int
number_held day ts = sum $ map calculateChangeToHolding ts
    where 
    calculateChangeToHolding (Transaction transaction_date shares_bought _)
        | transaction_date > day = 0
        | otherwise = shares_bought
        
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
