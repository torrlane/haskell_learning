module Utils
    (
    stripWhitespace, toByteString, toLazyByteString, epoch, toFiveDp, defaultWhenNull, parseInt, parseDouble, stripDoubleQuotes, (~=), delta, parseDate, listFilesInFolder, parseDateWithFormat
    )
where
import qualified Data.ByteString         as B (ByteString (..))
import qualified Data.ByteString.Builder as BB (stringUtf8, toLazyByteString)
import qualified Data.ByteString.Lazy    as LZ (ByteString (..), toStrict)
import           Data.Char               (isSpace)
import           Data.List               (dropWhileEnd)
import qualified Data.Text.Lazy          as L (pack)
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8)
import           Data.Time.Calendar      (Day (..), fromGregorian)
import           Data.Time.Format        (defaultTimeLocale, parseTimeOrError)
import           System.Directory        (listDirectory)
import           System.FilePath         (combine)

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
toFiveDp d = (/100000) $ fromIntegral $ round (d * 100000)

{-
 - Takes a default and a value and returns the default if value is null, the value otherwise
 -}
defaultWhenNull :: String -> String -> String
defaultWhenNull dfault str = if null str then dfault else str

parseDate :: String -> Day
parseDate s = parseDateWithFormat "%d/%m/%Y" s

parseDateWithFormat :: String -> String -> Day
parseDateWithFormat f s = parseTimeOrError True defaultTimeLocale f s :: Day

parseInt :: String -> Int
parseInt s = read s :: Int

parseDouble :: String -> Double
parseDouble s = read (stripChar ',' s) :: Double

{- Returns the original String with all instances of Char removed -}
stripChar :: Char -> String -> String
stripChar c =  filter (/= c)

stripDoubleQuotes :: String -> String
stripDoubleQuotes = stripChar '\"'

{-
 - returns a list of all the files in the specified folder
 -}
listFilesInFolder :: FilePath -> IO [FilePath]
listFilesInFolder folder = do
    putStrLn $ "Reading files from folder" ++ folder
    files <- listDirectory folder
    --take the folder and the array of files and prefix the files with the folder to give an array of absolute paths.
    let combined = map (combine folder) files
    return combined
