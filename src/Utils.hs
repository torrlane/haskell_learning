module Utils
    (
    ShareName, FileContent, convertToLazyByteString, dQuote, stripTextWhitespace, toByteString, epoch, toFiveDp, toTwoDp, defaultWhenNull, parseInt, parseDouble, stripDoubleQuotes, (~=), delta, parseDate, listFilesInFolder, parseDateWithFormat
    )
where
import qualified Data.ByteString         as B (ByteString (..), unpack)
import qualified Data.ByteString.Builder as BB (stringUtf8, toLazyByteString)
import qualified Data.ByteString.Lazy    as LZ (ByteString (..), pack, toStrict)
import           Data.Char               (isSpace)
import qualified Data.Text               as T (Text, cons, dropWhile,
                                               dropWhileEnd, snoc, unpack)
import qualified Data.Text.Lazy          as L (pack)
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8)
import           Data.Time.Calendar      (Day (..), fromGregorian)
import           Data.Time.Format        (defaultTimeLocale, parseTimeOrError)
import           System.Directory        (listDirectory)
import           System.FilePath         (combine)


type ShareName = T.Text
type FileContent = T.Text

{-
 - Removes the whitespace from the start and end of the String.
 - Not particularly efficient so don't use it with long Strings.
 -}
stripTextWhitespace :: T.Text -> T.Text
stripTextWhitespace s = T.dropWhile isSpace $ T.dropWhileEnd isSpace s

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


convertToLazyByteString :: B.ByteString -> LZ.ByteString
convertToLazyByteString s = LZ.pack $ B.unpack s


epoch :: Day
epoch = fromGregorian 1970 1 1


-- | Rounds the input to 5 decimal places
toFiveDp :: Double -> Double
toFiveDp d = (/100000) $ fromIntegral $ round (d * 100000)

toTwoDp :: Double -> Double
toTwoDp d = (/100) $ fromIntegral $ round (d * 100)
{-
 - Takes a default and a value and returns the default if value is null, the value otherwise
 -}
defaultWhenNull :: String -> String -> String
defaultWhenNull dfault str = if null str then dfault else str

parseDate :: String -> Day
parseDate = parseDateWithFormat "%d/%m/%Y"

parseDateWithFormat :: String -> String -> Day
parseDateWithFormat f s = parseTimeOrError True defaultTimeLocale f s :: Day

parseInt :: String -> Int
parseInt s = read s :: Int

parseDouble :: T.Text -> Double
parseDouble s = read (stripChar ',' (T.unpack s)) :: Double

{- Returns the original String with all instances of Char removed -}
stripChar :: Char -> String -> String
stripChar c =  filter (/= c)

stripDoubleQuotes :: String -> String
stripDoubleQuotes = stripChar '\"'

dQuote :: T.Text -> T.Text
dQuote t = T.cons '\"' $ T.snoc t '\"'

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
