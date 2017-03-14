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
        parseHolding
    ) where
import Data.Time.Calendar (Day(..))


data Transaction = Transaction{ transaction_date :: Day, shares_bought:: Int, cost :: Double } deriving (Read, Show, Eq)

data Dividend = Dividend{ paid_on :: Day, amount :: Double } deriving (Read, Show, Eq)

data Holding = Holding{ share :: String, transactions :: [Transaction], dividends :: [Dividend] } deriving (Read, Show, Eq)


{- Gives the amount of dividends paid up to the specified date (inclusive) -}
dividends_paid_upto :: Day -> [Dividend] -> [Transaction] -> Double
dividends_paid_upto d ds ts = sum $ map (dividend_amount d) ds
--dividends_paid_upto day (d:ds) ts = (dividend_amount day d) + dividends_paid_upto day ds ts
    where 
    dividend_amount day (Dividend paid_on amount)
        | paid_on > day = 0
        | otherwise = fromIntegral (number_held day ts) * amount

{- the number of shares held on the specified date (inclusive)-}
number_held :: Day -> [Transaction] -> Int
number_held day ts = sum $ map calculateChangeToHolding ts
--number_held (t:ts) day =  calculateChangeToHolding t + number_held ts day 
    where 
    calculateChangeToHolding (Transaction transaction_date shares_bought _)
        | transaction_date > day = 0
        | otherwise = shares_bought
        
parseHolding :: String -> Holding
parseHolding s = read s :: Holding 
