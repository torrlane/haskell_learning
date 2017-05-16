module Hl.Csv.Dividend
    (
        Dividend(Dividend, paid_on, amount),
    )
where
import Utils                            ((~=), delta)
import Data.Time.Calendar               (Day)



data Dividend = Dividend{ paid_on :: Day, amount :: Double } deriving (Read, Show)

-- equals doesn't work well for Doubles. Make Dividend an instance of Eq and use a small error when comparing the Dividend amounts
instance Eq Dividend where
    a == b = (paid_on a) == (paid_on b) && (amount a) ~= (amount b)
