module Hl.Csv.Transaction
    (
    Transaction(Transaction, actionedOn, sharesBought, cost),
    numberHeld
    )
where
import           Control.Monad      (mzero)
import           Data.Csv           (FromRecord (parseRecord), Parser, (.!))
import           Data.Time.Calendar (Day)
import           Utils              (parseDate)


{-
 - cost is the total cost of the Transaction, not the individual cost per unit
 - -}
data Transaction = Transaction{ actionedOn :: Day, sharesBought:: Int, cost :: Double } deriving (Read, Show, Eq)


{- the round is to convert from a Double i.e "123.00" to an Integer. Unfortunately, to get it to work, it needs to cast the input to a Double.
 -
 - This code is using Applicative Functor style. The <$> functions take a function from a -> b and an instance of Applicative Functor f a (say) and returns f b. Now in this case, it b is a function, so the <*>'s provide arguments to b, but wrapped in the Applicative f. It's not as complicated as it sounds!
 -}
instance FromRecord Transaction where
    parseRecord v
            | length v >= 6 = Transaction <$> (parseDate <$> v .! 0) <*> (round <$> (v .! 4 :: Parser Double) ) <*> v .! 5
            | otherwise     = mzero



{- the number of shares held on the specified date (inclusive)-}
numberHeld :: Day -> [Transaction] -> Int
numberHeld day ts = sum $ map calculateChangeToHolding ts
    where
    calculateChangeToHolding (Transaction transaction_date sharesBought _)
        | transaction_date > day = 0
        | otherwise = sharesBought
