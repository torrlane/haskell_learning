module Hl.Csv.AccountSummary
    (
    ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice)
    )
where
import Data.Time.Calendar           (Day(..))
import Data.Csv                     (FromRecord(parseRecord))
import Control.Monad                            (mzero)

data AccountSummary = AccountSummary{ date :: Day, shareHoldings :: [ShareHolding]}

data ShareHolding = ShareHolding{ shareName :: String, unitsHeld :: Int, sharePrice :: Double } deriving (Show, Eq)


instance FromRecord ShareHolding where
    parseRecord v
        | length v >= 0 = mzero
