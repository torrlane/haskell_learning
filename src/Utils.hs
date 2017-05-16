module Utils
    (
    stripWhitespace, toByteString, toLazyByteString, epoch, toFiveDp, defaultWhenNull, parseInt, parseDouble, stripDoubleQuotes, (~=), delta
    )
where
import Data.Char                                    (isSpace)
import Data.List                                    (dropWhileEnd)
import qualified Data.ByteString as B               (ByteString(..))
import qualified Data.ByteString.Lazy as LZ         (ByteString(..), toStrict)
import qualified Data.ByteString.Builder as BB      (toLazyByteString, stringUtf8)
import qualified Data.Text.Lazy.Encoding as E       (encodeUtf8)
import qualified Data.Text.Lazy as L                (pack)
import Data.Time.Calendar                           (Day(..), fromGregorian)

{-
 - Removes the whitespace from the start and end of the String. 
 - Not particularly efficient so don't use it with long Strings.
 -}
stripWhitespace :: String -> String 
stripWhitespace s = dropWhile isSpace $ dropWhileEnd isSpace s 

{- 'roughly equal' evaluates to true if the two input values are roughly equivalent -}
(~=) :: Double -> Double -> Bool
x ~= y = (x + delta) > y && (x - delta) < y

delta :: Double
delta = 0.000001

{-
 - Convert a String to a Data.ByteString.Internal.ByteString .
 -}
toByteString :: String -> B.ByteString
toByteString = LZ.toStrict . E.encodeUtf8 . L.pack 

{- 
 - Convert a String to a Data.ByteString.Lazy.Internal.ByteString
 -}
toLazyByteString :: String -> LZ.ByteString
toLazyByteString = BB.toLazyByteString . BB.stringUtf8 

epoch :: Day
epoch = fromGregorian 1970 1 1

{-
 - Rounds the input to 5 decimal places
 -}
toFiveDp :: Double -> Double
toFiveDp d = ((/100000) $ fromIntegral $ round (d * 100000))

{-
 - Takes a default and a value and returns the default if value is null, the value otherwise
 -}
defaultWhenNull :: String -> String -> String
defaultWhenNull dfault str = if null str then dfault else str

parseInt :: String -> Int
parseInt s = read s :: Int

parseDouble :: String -> Double
parseDouble s = read s :: Double

stripDoubleQuotes :: String -> String
stripDoubleQuotes xs = filter (\x -> x/= '"') xs
