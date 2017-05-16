module Hl.Csv.Transaction
    (
    Transaction(Transaction, actioned_on, shares_bought, cost),
    number_held
    )
where
import Data.Time.Calendar               (Day)


{- 
 - cost is the total cost of the Transaction, not the individual cost per unit
 - -}
data Transaction = Transaction{ actioned_on :: Day, shares_bought:: Int, cost :: Double } deriving (Read, Show, Eq)

{- the number of shares held on the specified date (inclusive)-}
number_held :: Day -> [Transaction] -> Int
number_held day ts = sum $ map calculateChangeToHolding ts
    where 
    calculateChangeToHolding (Transaction transaction_date shares_bought _)
        | transaction_date > day = 0
        | otherwise = shares_bought
