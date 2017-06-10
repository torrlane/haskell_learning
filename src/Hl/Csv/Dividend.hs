module Hl.Csv.Dividend
    (
        Dividend(Dividend, paid_on, amount),
    )
where
import Utils                                    ((~=), delta, toFiveDp, parseDate)
import Control.Monad                            (mzero)
import Data.Time.Calendar                       (Day)
import Data.Csv                                 (FromRecord(parseRecord), Parser, (.!))


data Dividend = Dividend{ paid_on :: Day, amount :: Double } deriving (Read, Show)

-- equals doesn't work well for Doubles. Make Dividend an instance of Eq and use a small error when comparing the Dividend amounts
instance Eq Dividend where
    a == b = paid_on a == paid_on b && amount a ~= amount b


{- The zeroth field contains the date that the dividend was paid on, the 4th field the number of shares held, and the 5th field the total amount paid. We divide the total amount by the number of shares, to get a dividend amount per share (in pence, to 5 dp)
 -}
instance FromRecord Dividend where
    parseRecord v
            | length v >= 6 = Dividend <$> (parseDate <$> v .! 0) <*> ( toFiveDp . (100*) <$> ( (/) <$> (v .! 5 :: Parser Double) <*> (v .! 4 :: Parser Double)))
            | otherwise     = mzero

