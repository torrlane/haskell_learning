module ParseDividends where
import AltLib (Dividend(..))
import Data.Char(isSpace)
import Data.List (null, drop, stripPrefix, dropWhileEnd)
import Data.Time.Calendar
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Data.Csv
import Data.Either.Combinators (fromRight)
import Control.Monad (mzero)
import Data.Vector (Vector(..), toList, empty)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder (toLazyByteString, stringUtf8)
import Data.Text.Encoding (encodeUtf8)

stripWhitespace :: String -> String 
stripWhitespace s = dropWhile isSpace $ dropWhileEnd isSpace s 

getShareName :: String -> Maybe String
getShareName s  
    | null (shareName s) = Nothing
    | otherwise     = Just $ shareName s
    where shareName  = stripWhitespace . (takeWhile (','/=)) . tail . (dropWhile (','/=) ) 

{- The zeroth field contains the date that the dividend was paid on, the 4th field the number of shares held, and the 5th field the total amount paid. We divide the total amount by the number of shares, to get a dividend amount per share (in pence, to 5 dp)
 -}
instance FromRecord Dividend where
    parseRecord v
            | length v >= 6 = Dividend <$> (parseDate <$> v .! 0) <*> ( to_five_dp . (100*) <$> ( (/) <$> (v .! 5 :: Parser Double) <*> (v .! 4 :: Parser Double)))
            | otherwise     = mzero



to_five_dp :: Double -> Double
to_five_dp d = ((/100000) $ fromIntegral $ round (d * 100000))

parseDate :: String -> Day
parseDate s = parseTimeOrError True defaultTimeLocale "%d/%m/%Y" s :: Day

{- Removes the header section from the transactions csv file, returning just the csv lines -}
removeTransactionHeader :: String -> B.ByteString
removeTransactionHeader = toLazyByteString . stringUtf8 . unlines . (drop 9) . lines

removeDividendHeader :: String -> B.ByteString
removeDividendHeader = toLazyByteString . stringUtf8 . unlines . (drop 9) . lines

decodeTransactions :: String -> Either String (Vector Transaction)
decodeTransactions str = decode NoHeader (removeTransactionHeader str) :: Either String (Vector Transaction)

parseTransactions :: String -> [Transaction]
parseTransactions str =  toList $ fromRight empty $ decodeTransactions str

decodeDividends :: String -> Either String (Vector Dividend)
decodeDividends str = decode NoHeader (removeDividendHeader str) :: Either String (Vector Dividend)

parseDividends :: String -> [Dividend]
parseDividends str = toList $ fromRight empty $ decodeDividends str


{- the round is to convert from a Double i.e "123.00" to an Integer. Unfortunately, to get it to work, it needs to cast the input to a Double.
 -
 - This code is using Applicative Functor style. The <$> functions take a function from a -> b and an instance of Applicative Functor f a (say) and returns f b. Now in this case, it b is a function, so the <*>'s provide arguments to b, but wrapped in the Applicative f. It's not as complicated as it sounds!
 -}
instance FromRecord Transaction where
    parseRecord v
            | length v >= 6 = Transaction <$> (parseDate <$> v .! 0) <*> (round <$> (v .! 4 :: Parser Double) ) <*> v .! 5 
            | otherwise     = mzero


data Transaction = Transaction{ transaction_date :: Day, shares_bought:: Int, cost :: Double } deriving (Read, Show, Eq)
