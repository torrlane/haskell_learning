module Hl.Csv.Dividend
    (
        Dividend(Dividend, paidOn, amount),
        getDividends
    )
where
import           Control.Monad           (mzero)
import           Data.Csv                (FromRecord (parseRecord), Parser,
                                          (.!))
import           Data.Either.Combinators (fromRight)
import           Data.Map                as M (Map, empty, fromList, union)
import           Data.Time.Calendar      (Day)
import           Data.Vector             as V (empty, toList)
import           ParseCsv                (decodeCsv, getShareName)
import           Utils                   (delta, listFilesInFolder, parseDate,
                                          toFiveDp, (~=))


data Dividend = Dividend{ paidOn :: Day, amount :: Double } deriving (Read, Show)

-- equals doesn't work well for Doubles. Make Dividend an instance of Eq and use a small error when comparing the Dividend amounts
instance Eq Dividend where
    a == b = paidOn a == paidOn b && amount a ~= amount b

getDividends :: FilePath -> IO (M.Map String [Dividend])
getDividends dividendsFolder = do
    dividendFiles <- listFilesInFolder dividendsFolder
    buildMap parseDividends dividendFiles

{- Takes a parser function and a list of files and produces a map from the share name to the lists of the parsed values
 -}
buildMap :: (FromRecord a) => (String -> [a]) -> [FilePath] -> IO (M.Map String [a])
buildMap parser = foldl acc (return M.empty)
    where acc ioMap f = do
            newMap <- buildMapFromCsv parser f
            accMap <- ioMap
            return $ union accMap newMap
{- takes a csv file and returns a map from the sharename to a list of dividends/transactions/... from the file
 -}
buildMapFromCsv :: (FromRecord a) => (String -> [a]) -> FilePath -> IO (M.Map String [a])
buildMapFromCsv parser file = do
    contents <- readFile file
    let contentLines = lines contents
    let shareName = getShareName . head $ contentLines
    let values = parser contents
    return $ mapMaybe shareName values
    where
    mapMaybe Nothing v  = M.empty
    mapMaybe (Just k) v = fromList [(k,v)]

parseDividends :: String -> [Dividend]
parseDividends str = V.toList $ fromRight V.empty $ decodeCsv dividendHeader str

dividendHeader :: Int
dividendHeader = 9

{- The zeroth field contains the date that the dividend was paid on, the 4th field the number of shares held, and the 5th field the total amount paid. We divide the total amount by the number of shares, to get a dividend amount per share (in pence, to 5 dp)
 -}
instance FromRecord Dividend where
    parseRecord v
            | length v >= 6 = Dividend <$> (parseDate <$> v .! 0) <*> ( toFiveDp . (100*) <$> ( (/) <$> (v .! 5 :: Parser Double) <*> (v .! 4 :: Parser Double)))
            | otherwise     = mzero

