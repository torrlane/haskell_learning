module Utils
    (
    stripWhitespace, toByteString, toLazyByteString, epoch
    )
where
import Data.Char(isSpace)
import Data.List (dropWhileEnd)
import qualified Data.ByteString as B (ByteString(..))
import qualified Data.ByteString.Lazy as LZ (ByteString(..), toStrict)
import qualified Data.ByteString.Builder as BB (toLazyByteString, stringUtf8)
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8)
import qualified Data.Text.Lazy as L (pack)
import Data.Time.Calendar (Day(..), fromGregorian)

{-
 - Removes the whitespace from the start and end of the String. 
 - Not particularly efficient so don't use it with long Strings.
 -}
stripWhitespace :: String -> String 
stripWhitespace s = dropWhile isSpace $ dropWhileEnd isSpace s 


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
