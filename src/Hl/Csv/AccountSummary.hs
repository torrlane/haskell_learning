module Hl.Csv.AccountSummary
    (
    AccountSummary(date),
    ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice),
    findShareHolding,
    holdingValue,
    parseAccountSummary
    )
where
import           Control.Monad      (mzero)
import           Data.Csv           (FromRecord (parseRecord), Parser, (.!))
import           Data.Either.Combinators (fromRight)
import           Data.List          (drop, find, isPrefixOf, length, lines, take, zip)
import           Data.Time.Calendar (Day (..))
import           Data.Vector        as V (empty, toList)
import           ParseCsv           (decodeCsv, getShareName)
import           Utils              (listFilesInFolder, parseDateWithFormat, parseDouble, parseInt, stripDoubleQuotes)

data AccountSummary = AccountSummary{ date :: Day, shareHoldings :: [ShareHolding]} deriving (Show, Eq)

-- | contains details of how many of a particular share is held at a certain point in time, and the shareprice(in pence) at that time
data ShareHolding = ShareHolding{ shareName :: String, unitsHeld :: Double, sharePrice :: Double } deriving (Show, Eq)

-- | the total value of the holding (in pounds) e.g. sharePrice * unitsHeld / 100
holdingValue :: ShareHolding -> Double
holdingValue (ShareHolding{unitsHeld=h, sharePrice=p}) = (h * p)/100

-- | takes a name of a share and an AccountSummary and returns the first ShareHolding
-- where the name of the share in the ShareHolding is a prefix of the input String.
-- This is because the HL AccountSummary adds stuff like "25 *1" to the shareName.
findShareHolding :: String -> AccountSummary -> Maybe ShareHolding
findShareHolding share (AccountSummary{shareHoldings=shs}) = find (\s -> isPrefixOf share (shareName s)) shs

parseAccountSummary :: String -> AccountSummary
parseAccountSummary accountSummaryContents = AccountSummary{date=asDate, shareHoldings = holdings}
  where asDate = getAccountSummaryDate accountSummaryContents
        holdings = parseShareHoldings accountSummaryContents

-- | takes the csv content of the accountSummary and extracts the date
-- TODO - this returns the epoch date if there are problems with parsing. It should return an Either.
getAccountSummaryDate :: String -> Day
getAccountSummaryDate content = parseDateWithFormat "%d-%m-%Y" unparsed
    where prefix = "Spreadsheet created at,"
          mapMaybe Nothing = prefix ++ "01-01-1970"
          mapMaybe (Just s) = s
          line = mapMaybe $ find (isPrefixOf prefix ) (lines content)
          unparsed = take 10 $ drop (length prefix) line

shareHoldingHeader :: Int
shareHoldingHeader = 11

parseShareHoldings :: String -> [ShareHolding]
parseShareHoldings str = V.toList $ fromRight V.empty $ decodeCsv shareHoldingHeader str

instance FromRecord ShareHolding where
    parseRecord v
        | length v >= 3 = ShareHolding <$> strip (v .! 0) <*> (parseDouble <$> strip (v .! 1)) <*> (parseDouble <$> strip (v .! 2))
        | otherwise = mzero

strip :: Parser String -> Parser String
strip p = stripDoubleQuotes <$> p



