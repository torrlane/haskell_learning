module ParseCsv 
    (
    getShareName, parseDividends, parseTransactions
    )
where
import Hl.Csv.Dividend                          (Dividend(Dividend))
import Hl.Csv.Transaction                       (Transaction(Transaction))
import Utils                                    (stripWhitespace, toLazyByteString, toFiveDp, parseDate)
import Data.List                                (null, drop, stripPrefix)
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
    where shareName  = stripWhitespace . (takeWhile (','/=)) . tail . (dropWhile (','/=) ) 



{- Removes the header section from the transactions csv file, returning just the csv lines -}
removeCsvHeader :: String -> B.ByteString
removeCsvHeader = toLazyByteString . unlines . (drop 9) . lines

parseTransactions :: String -> [Transaction]
parseTransactions str =  toList $ fromRight empty $ decodeCsv str

{- 
 - Takes a String, removes any header lines, and then parses the remaining lines into instances of Type a
 -}
decodeCsv :: FromRecord a => String -> Either String (Vector a)
decodeCsv str = decode NoHeader (removeCsvHeader str) 

parseDividends :: String -> [Dividend]
parseDividends str = toList $ fromRight empty $ decodeCsv str


