module ParseCsv
    (
    getShareName, parseDividends, decodeCsv, parseShareHoldings
    )
where
import           Control.Monad           (mzero)
import qualified Data.ByteString.Lazy    as B (ByteString)
import           Data.Csv                (FromRecord (parseRecord),
                                          HasHeader (NoHeader), Parser, Record,
                                          decode, (.!))
import           Data.Either.Combinators (fromRight)
import           Data.List               (drop, isPrefixOf, null, stripPrefix,
                                          takeWhile)
import           Data.Time.Calendar      (Day)
import           Data.Time.Format        (defaultTimeLocale, parseTimeOrError)
import           Data.Vector             (Vector, empty, toList)
import           Hl.Csv.AccountSummary   (ShareHolding)
import           Hl.Csv.Dividend         (Dividend)
import           Utils                   (parseDate, stripWhitespace, toFiveDp,
                                          toLazyByteString)

getShareName :: String -> Maybe String
getShareName s
    | null (shareName s) = Nothing
    | otherwise     = Just $ shareName s
    where shareName  = stripWhitespace . takeWhile (','/=) . tail . dropWhile (','/=)

{- Removes the header section from the transactions csv file, returning just the csv lines -}
stripHeader :: Int -> String -> String
stripHeader h = unlines . drop h . lines

stripFooter :: String -> String
stripFooter = unlines . takeWhile (not . isPrefixOf "\"Totals\"") . lines

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

parseShareHoldings :: String -> [ShareHolding]
parseShareHoldings str = toList $ fromRight empty $ decodeCsv shareHoldingHeader str
