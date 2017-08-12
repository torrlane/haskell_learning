module Hl.Csv.AccountSummary
    (
    ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice)
    )
where
import           Control.Monad      (mzero)
import           Data.Csv           (FromRecord (parseRecord), Parser, (.!))
import           Data.Time.Calendar (Day (..))
import           Utils              (parseDouble, parseInt, stripDoubleQuotes)

data AccountSummary = AccountSummary{ date :: Day, shareHoldings :: [ShareHolding]}

-- | contains details of how many of a particular share is held at a certain point in time, and the shareprice at that time
data ShareHolding = ShareHolding{ shareName :: String, unitsHeld :: Double, sharePrice :: Double } deriving (Show, Eq)


instance FromRecord ShareHolding where
    parseRecord v
        | length v >= 3 = ShareHolding <$> strip (v .! 0) <*> (parseDouble <$> strip (v .! 1)) <*> (parseDouble <$> strip (v .! 2))
        | otherwise = mzero

strip :: Parser String -> Parser String
strip p = stripDoubleQuotes <$> p

-- | TODO getShareHolding on Date, for share - search for shareholdings where the search string is a prefix of the shareName. This is because the accountSummary adds stuff like "25 *1" to the end of the sharename.
