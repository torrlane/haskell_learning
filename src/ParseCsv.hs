module ParseCsv
    (
    getShareName, parseDividends, parseTransactions, parseShareHoldings
    )
where
import Hl.Csv.Dividend                          (Dividend)
import Hl.Csv.Transaction                       (Transaction)
import Hl.Csv.AccountSummary                    (ShareHolding)
import Utils                                    (stripWhitespace, toLazyByteString, toFiveDp, parseDate)
import Data.List                                (null, drop, stripPrefix, isPrefixOf, takeWhile)
import Data.Time.Calendar                       (Day)
import Data.Time.Format                         (parseTimeOrError, defaultTimeLocale)
import Data.Csv                                 (FromRecord(parseRecord), Parser, Record, HasHeader(NoHeader), (.!), decode)
import Data.Either.Combinators                  (fromRight)
import Control.Monad                            (mzero)
import Data.Vector                              (Vector, toList, empty )
import qualified Data.ByteString.Lazy as B      (ByteString)

getShareName :: String -> Maybe String
getShareName s
    | null (shareName s) = Nothing
    | otherwise     = Just $ shareName s
    where shareName  = stripWhitespace . takeWhile (','/=) . tail . dropWhile (','/=)

{- Removes the header section from the transactions csv file, returning just the csv lines -}
stripHeader :: Int -> String -> String
stripHeader h = unlines . drop h . lines

stripFooter :: String -> String
stripFooter = unlines . takeWhile (\s -> not (isPrefixOf "\"Totals\"" s)) . lines

transactionHeader :: Int
transactionHeader = 9

dividendHeader :: Int
dividendHeader = 9

shareHoldingHeader :: Int
shareHoldingHeader = 11

{- 
 - Takes a String, removes any header lines, and then parses the remaining lines into instances of Type a
 -}
decodeCsv :: FromRecord a => Int -> String -> Either String (Vector a)
decodeCsv h str = decode NoHeader $ toLazyByteString . stripHeader h . stripFooter $ str

parseDividends :: String -> [Dividend]
parseDividends str = toList $ fromRight empty $ decodeCsv dividendHeader str

parseTransactions :: String -> [Transaction]
parseTransactions str =  toList $ fromRight empty $ decodeCsv transactionHeader str

parseShareHoldings :: String -> [ShareHolding]
parseShareHoldings str = toList $ fromRight empty $ decodeCsv shareHoldingHeader str
