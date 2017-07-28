module ParseCsvMain where
import           Data.Char          (isSpace)
import           Data.Csv           (FromRecord)
import           Data.List          (dropWhile, dropWhileEnd, length, lines)
import           Data.Map           as M (Map, empty, fromList, union)
import           Hl.Csv.Transaction (HlCsvTransactionDao (HlCsvTransactionDao),
                                     getTransactions)
import           Lib                (createHoldings)
import           ParseCsv           (getShareName, parseDividends,
                                     parseShareHoldings)
import           System.Directory   (getHomeDirectory, listDirectory)
import           System.FilePath    (combine)
import           System.IO          (BufferMode (LineBuffering),
                                     IOMode (ReadMode), hClose, hGetContents,
                                     hSetBuffering, openFile, putStrLn, stdout)
import           Utils              (defaultWhenNull, listFilesInFolder,
                                     stripWhitespace)

main :: IO ()
main = do
-- set stdout to use linebuffering so that get/print IO actions work as expected
    hSetBuffering stdout LineBuffering
    home <- getHomeDirectory
    let defaultDividendFolder = home ++ "/Downloads/Dividends/"
    let defaultTransactionFolder = home ++ "/Downloads/Transactions/"
    let defaultAccountSummaryFolder = home ++ "/Downloads/AccountSummary"

    dividendsFolder <- requestFolder "Please provide a dividends folder" defaultDividendFolder
    dividendFiles <- listFilesInFolder dividendsFolder
    dividendsMap <- buildMap parseDividends dividendFiles
    mapM_ (printCsvData parseDividends) dividendFiles

    transactionsFolder <- requestFolder "Please provide a transactions folder" defaultTransactionFolder
    let transactionDao = HlCsvTransactionDao transactionsFolder
    transactions <- getTransactions transactionDao
    mapM_ print transactions
    --transactionFiles <- listFilesInFolder transactionsFolder
    --transactionsMap <- buildMap parseTransactions transactionFiles
    --mapM_ (printCsvData parseTransactions) transactionFiles

    accountSummaryFolder <- requestFolder "Please provide an accountSummary folder" defaultAccountSummaryFolder
    accountSummaryFiles <- listFilesInFolder accountSummaryFolder
    let accountSummaryFile = head accountSummaryFiles
    putStrLn $ "accountSummaryFile: " ++ accountSummaryFile
    contents <- readFile accountSummaryFile
    let shareHoldings = parseShareHoldings contents
    putStrLn $ (show . length) shareHoldings ++ " shareHoldings found"
    mapM_ print shareHoldings

    --putStrLn "Holdings"
    --let holdings = createHoldings transactionsMap dividendsMap
    --mapM_ print holdings


{-
 - Takes a question to ask the user i.e "please provide a folder", and a default value.
 - Asks the user the question an returns their answer or the default if they answered with null.
 -}
requestFolder :: String -> String -> IO FilePath
requestFolder prompt dfault = do
    putStrLn $ prompt ++ " [" ++ dfault ++ "]"
    input <- getLine
    let folder = stripWhitespace $ defaultWhenNull dfault input
    return folder


printShareName :: Maybe String -> IO()
printShareName Nothing  = putStrLn "Could not find share name"
printShareName (Just s) = putStrLn $ "Found share: " ++ s

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
    mapMaybe Nothing v  = empty
    mapMaybe (Just k) v = fromList [(k,v)]

printCsvData :: (FromRecord a, Show a) => (String -> [a]) -> FilePath -> IO()
printCsvData parser fullFileName = do
    putStrLn $ "reading data from " ++ fullFileName
    handle <- openFile fullFileName ReadMode
    contents <- hGetContents handle
    let contentLines = lines contents
    printShareName $ getShareName . head $ contentLines
    mapM_ print (parser contents)
    hClose handle




