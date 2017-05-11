module ParseCsvMain where
import Utils (stripWhitespace, defaultWhenNull)
import Data.Csv (FromRecord)
import Data.List(lines, dropWhileEnd, dropWhile)
import Data.Map as M (Map(..), union, empty, fromList)
import Data.Char(isSpace)
import System.IO(putStrLn, openFile, IOMode(ReadMode), hClose, hSetBuffering, stdout, BufferMode(LineBuffering), hGetContents )
import System.Directory(listDirectory, getHomeDirectory)
import ParseCsv
import System.FilePath(combine)
import AltLib(Dividend(..), Transaction(..), Holding(..), createHoldings)

main :: IO ()
main = do  
-- set stdout to use linebuffering so that get/print IO actions work as expected
    hSetBuffering stdout LineBuffering
    home <- getHomeDirectory
    let defaultDividendFolder = home ++ "/Downloads/Dividends/"
    let defaultTransactionFolder = home ++ "/Downloads/Transactions/"
    dividendsFolder <- requestFolder "Please provide a dividends folder" defaultDividendFolder
    dividendFiles <- absolutePaths dividendsFolder $ listFilesInFolder dividendsFolder
    dividendsMap <- buildMap parseDividends dividendFiles 
    mapM_ (printCsvData parseDividends) dividendFiles
    transactionsFolder <- requestFolder "Please provide a transactions folder" defaultTransactionFolder
    transactionFiles <- absolutePaths transactionsFolder $ listFilesInFolder transactionsFolder
    transactionsMap <- buildMap parseTransactions transactionFiles 
    mapM_ (printCsvData parseTransactions) transactionFiles
    let holdings = createHoldings transactionsMap dividendsMap
    mapM_ (putStrLn . show) holdings


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

{- 
 - returns a list of all the files in the specified folder
 -}
listFilesInFolder :: FilePath -> IO [FilePath]
listFilesInFolder folder = do
    putStrLn $ "Reading files from folder" ++ folder
    files <- listDirectory folder
    return files

absolutePaths :: FilePath -> IO [FilePath] -> IO [FilePath]
absolutePaths folder ioFiles = do
    files <- ioFiles
    let combined = map (combine folder) files
    return combined 

    
printShareName :: Maybe String -> IO()
printShareName Nothing = putStrLn "Could not find share name"
printShareName (Just s) = putStrLn $ "Found share: " ++ s

{- Takes a parser function and a list of files and produces a map from the share name to the lists of the parsed values
 -}
buildMap :: (FromRecord a) => (String -> [a]) -> [FilePath] -> IO (M.Map String [a])
buildMap parser fs = foldl acc (return M.empty) fs
    where acc ioMap f = do
            newMap <- buildMapFromCsv parser f
            accMap <- ioMap
            return $ union accMap newMap

{- takes a csv file and returns a map from the sharename to a list of dividends/transactions/... from the file
 -}
buildMapFromCsv :: (FromRecord a) => (String -> [a]) -> FilePath -> IO (M.Map String [a])
buildMapFromCsv parser file = do
    putStrLn $ "reading data from: " ++ file
    handle <- openFile file ReadMode
    contents <- hGetContents handle  
    let contentLines = lines contents
    let shareName = getShareName . head $ contentLines
    let values = parser contents
    return $ mapMaybe shareName values
    where
    mapMaybe Nothing v = empty
    mapMaybe (Just k) v = fromList [(k,v)]


printCsvData :: (FromRecord a, Show a) => (String -> [a]) -> FilePath -> IO()
printCsvData parser fullFileName = do
    putStrLn $ "reading data from " ++ fullFileName
    handle <- openFile fullFileName ReadMode
    contents <- hGetContents handle  
    let contentLines = lines contents
    printShareName $ getShareName . head $ contentLines
    mapM_ putStrLn $ map show $ parser contents
    hClose handle




