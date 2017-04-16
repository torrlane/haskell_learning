module ParseDividends 
    (
    getShareName, parseDividends, parseTransactions
    )
where
import AltLib (Dividend(Dividend))
import Utils (stripWhitespace, toLazyByteString, toFiveDp)
import Data.List (null, drop, stripPrefix)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Data.Csv (FromRecord(parseRecord), Parser, Record, HasHeader(NoHeader), (.!), decode)
import Data.Either.Combinators (fromRight)
import Control.Monad (mzero)
import Data.Vector (Vector, toList, empty )
import qualified Data.ByteString.Lazy as B (ByteString)


getShareName :: String -> Maybe String
getShareName s  
    | null (shareName s) = Nothing
    | otherwise     = Just $ shareName s
    where shareName  = stripWhitespace . (takeWhile (','/=)) . tail . (dropWhile (','/=) ) 

{- The zeroth field contains the date that the dividend was paid on, the 4th field the number of shares held, and the 5th field the total amount paid. We divide the total amount by the number of shares, to get a dividend amount per share (in pence, to 5 dp)
 -}
instance FromRecord Dividend where
    parseRecord v
            | length v >= 6 = Dividend <$> (parseDate <$> v .! 0) <*> ( toFiveDp . (100*) <$> ( (/) <$> (v .! 5 :: Parser Double) <*> (v .! 4 :: Parser Double)))
            | otherwise     = mzero


parseDate :: String -> Day
parseDate s = parseTimeOrError True defaultTimeLocale "%d/%m/%Y" s :: Day

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


{- the round is to convert from a Double i.e "123.00" to an Integer. Unfortunately, to get it to work, it needs to cast the input to a Double.
 -
 - This code is using Applicative Functor style. The <$> functions take a function from a -> b and an instance of Applicative Functor f a (say) and returns f b. Now in this case, it b is a function, so the <*>'s provide arguments to b, but wrapped in the Applicative f. It's not as complicated as it sounds!
 -}
instance FromRecord Transaction where
    parseRecord v
            | length v >= 6 = Transaction <$> (parseDate <$> v .! 0) <*> (round <$> (v .! 4 :: Parser Double) ) <*> v .! 5 
            | otherwise     = mzero


data Transaction = Transaction{ transaction_date :: Day, shares_bought:: Int, cost :: Double } deriving (Read, Show, Eq)
