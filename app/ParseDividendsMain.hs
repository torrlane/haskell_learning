module ParseDividendsMain where
import Utils (stripWhitespace)
import Data.Csv (FromRecord)
import Data.List(lines, dropWhileEnd, dropWhile)
import Data.Char(isSpace)
import System.IO(putStrLn, openFile, IOMode(ReadMode), hClose, hSetBuffering, stdout, BufferMode(LineBuffering), hGetContents )
import System.Directory(listDirectory, getHomeDirectory)
import ParseDividends
import System.FilePath(combine)

main :: IO ()
main = do  
-- set stdout to use linebuffering so that get/print IO actions work as expected
    hSetBuffering stdout LineBuffering
    home <- getHomeDirectory
    let defaultDividendFolder = home ++ "/Downloads/Dividends/"
    let defaultTransactionFolder = home ++ "/Downloads/Transactions/"
    dividendsFolder <- requestFolder "Please provide a dividends folder" defaultDividendFolder
    dividendFiles <- listFilesInFolder dividendsFolder
    mapM_ (printCsvData parseDividends dividendsFolder) dividendFiles
    transactionsFolder <- requestFolder "Please provide a transactions folder" defaultTransactionFolder
    transactionFiles <- listFilesInFolder transactionsFolder
    mapM_ (printCsvData parseTransactions transactionsFolder) transactionFiles

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

{-
 - Takes a default and a value and returns the default if value is null, the value otherwise
 -}
defaultWhenNull :: String -> String -> String
defaultWhenNull dfault str = if null str then dfault else str
    
printShareName :: Maybe String -> IO()
printShareName Nothing = putStrLn "Could not find share name"
printShareName (Just s) = putStrLn $ "Found share: " ++ s

printCsvData :: (FromRecord a, Show a) => (String -> [a]) -> FilePath -> FilePath -> IO()
printCsvData parser folder fileName = do
    putStrLn $ "reading data from " ++ fileName
    let fullFileName = combine folder fileName
    handle <- openFile fullFileName ReadMode
    contents <- hGetContents handle  
    let contentLines = lines contents
    printShareName $ getShareName . head $ contentLines
    mapM_ putStrLn $ map show $ parser contents
    hClose handle
