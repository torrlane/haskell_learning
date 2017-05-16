module Hl.Csv.AccountSummary
    (
    ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice)
    )
where
import Data.Time.Calendar           (Day(..))
import Data.Csv                     (Parser, FromRecord(parseRecord), (.!))
import Control.Monad                (mzero)
import Utils                        (parseInt, parseDouble, stripDoubleQuotes)

data AccountSummary = AccountSummary{ date :: Day, shareHoldings :: [ShareHolding]}

data ShareHolding = ShareHolding{ shareName :: String, unitsHeld :: Int, sharePrice :: Double } deriving (Show, Eq)


instance FromRecord ShareHolding where
    parseRecord v
        | length v >= 3 = ShareHolding <$> (strip (v .! 0)) <*> (parseInt <$> strip (v .! 1)) <*> (parseDouble <$> strip (v .! 2))
        | length v >= 0 = mzero

strip :: Parser String -> Parser String
strip p = stripDoubleQuotes <$> p

