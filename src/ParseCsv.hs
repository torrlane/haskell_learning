module ParseCsv
    (
    buildMap, getShareName, decodeCsv
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
import           Data.Map                as M (Map, empty, union)
import           Data.Time.Calendar      (Day)
import           Data.Time.Format        (defaultTimeLocale, parseTimeOrError)
import           Data.Vector             (Vector, toList)
import           Data.Vector             as V (empty)
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

{- Takes a parser function and a list of files and produces a map from the share name to the lists of the parsed values
 -}
buildMap :: (FromRecord a) => (FilePath -> IO (M.Map String [a])) -> [FilePath] -> IO (M.Map String [a])
buildMap parseFile = foldl acc (return M.empty)
    where acc ioMap f = do
            newMap <- parseFile f
            accMap <- ioMap
            return $ union accMap newMap

{-
 - Takes a String, removes any header lines, and then parses the remaining lines into instances of Type a
 -}
decodeCsv :: FromRecord a => Int -> String -> Either String (Vector a)
decodeCsv h str = decode NoHeader $ toLazyByteString . stripHeader h . stripFooter $ str

