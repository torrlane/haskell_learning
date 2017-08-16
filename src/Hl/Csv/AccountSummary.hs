module Hl.Csv.AccountSummary
    (
    ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice),
    getAccountSummaries
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

-- | contains details of how many of a particular share is held at a certain point in time, and the shareprice at that time
data ShareHolding = ShareHolding{ shareName :: String, unitsHeld :: Double, sharePrice :: Double } deriving (Show, Eq)

getAccountSummaries :: FilePath -> IO [AccountSummary]
getAccountSummaries accountSummariesFolder = do
    accountSummaryFiles <- listFilesInFolder accountSummariesFolder
    accountSummaryContents <- sequence $ map readFile accountSummaryFiles
    let shareHoldingLists = map parseShareHoldings accountSummaryContents
    let accountSummaryDates = map getAccountSummaryDate accountSummaryContents
    let accountSummaryArgs = zip accountSummaryDates shareHoldingLists
    return $ map (\(d,ss) -> AccountSummary{date = d, shareHoldings = ss}) accountSummaryArgs

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

-- | TODO getShareHolding on Date, for share - search for shareholdings where the search string is a prefix of the shareName. This is because the accountSummary adds stuff like "25 *1" to the end of the sharename.
