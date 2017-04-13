module ParseDividends where
import AltLib (Dividend(..))
import Data.Char(isSpace)
import Data.List (null, drop, stripPrefix, dropWhileEnd)
import Data.List.Split (splitOn)
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

removeHeader :: [String] -> [String]
removeHeader = drop 9

-- takes a String and splits on commas
splitIntoFields :: String -> [String]
splitIntoFields s = splitOn "," s

selectFields :: [String] -> [String]
selectFields xs = [xs!!0, xs!!4, xs!!5]

convertFieldsToDividend :: [String] -> Dividend
convertFieldsToDividend xs = Dividend{paid_on=dividend_date, amount=dividend_amount}
    where dividend_date = parseDate (xs!!0)
          dividend_amount = to_five_dp $ 100 * (total_amount / num_shares)
            where num_shares = read (xs!!1) :: Double
                  total_amount = read (xs!!2) :: Double

to_five_dp :: Double -> Double
to_five_dp d = ((/100000) $ fromIntegral $ round (d * 100000))

parseDate :: String -> Day
parseDate s = parseTimeOrError True defaultTimeLocale "%d/%m/%Y" s :: Day

{- Removes the header section from the transactions csv file, returning just the csv lines -}
removeTransactionHeader :: String -> B.ByteString
removeTransactionHeader = toLazyByteString . stringUtf8 . unlines . (drop 9) . lines

decodeTransactions :: String -> Either String (Vector Transaction)
decodeTransactions str = decode NoHeader (removeTransactionHeader str) :: Either String (Vector Transaction)

parseTransactions :: String -> [Transaction]
parseTransactions str =  toList $ fromRight empty $ decodeTransactions str


{- the round is to convert from a Double i.e "123.00" to an Integer. Unfortunately, to get it to work, it needs to cast the input to a Double.
 -
 - This code is using Applicative Functor style. The <$> functions take a function from a -> b and an instance of Applicative Functor f a (say) and returns f b. Now in this case, it b is a function, so the <*>'s provide arguments to b, but wrapped in the Applicative f. It's not as complicated as it sounds!
 -}
instance FromRecord Transaction where
    parseRecord v
            | length v >= 6 = Transaction <$> (parseDate <$> v .! 0) <*> (round <$> (v .! 4 :: Parser Double) ) <*> v .! 5
            | otherwise     = mzero


data Transaction = Transaction{ transaction_date :: Day, shares_bought:: Int, cost :: Double } deriving (Read, Show, Eq)
