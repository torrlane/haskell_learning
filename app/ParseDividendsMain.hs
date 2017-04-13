module ParseDividendsMain where

import Data.List(lines, dropWhileEnd, dropWhile)
import Data.Char(isSpace)
import System.IO(putStrLn, openFile, IOMode(ReadMode), hClose, hSetBuffering, stdout, BufferMode(LineBuffering), hGetContents )
import System.Directory(listDirectory)
import ParseDividends
import System.FilePath(combine)

main :: IO ()
main = do  
-- set stdout to use linebuffering so that get/print IO actions work as expected
    hSetBuffering stdout LineBuffering
-- ask user for a folder with dividend details
    putStrLn "Please provide a dividends folder"
    input <- getLine
    let folderName= stripWhitespace input
    putStrLn $ "Reading files from folder" ++ folderName 
    dividendFiles <- listDirectory folderName
    mapM_ (readDividends folderName) dividendFiles
    putStrLn "please provide a transactions folder"
    input2 <- getLine
    let transactionsFolder = stripWhitespace input2
    putStrLn $ "Reading files from folder" ++ transactionsFolder
    transactionFiles <- listDirectory transactionsFolder
    mapM_ (readTransactions transactionsFolder) transactionFiles
    

-- read file in and print it

printShareName :: Maybe String -> IO()
printShareName Nothing = putStrLn "Could not find share name"
printShareName (Just s) = putStrLn $ "Found share: " ++ s

readTransactions :: FilePath -> FilePath -> IO()
readTransactions folder fileName = do
    putStrLn $ "reading transaction data from " ++ fileName
    let fullFileName = combine folder fileName
    handle <- openFile fullFileName ReadMode
    contents <- hGetContents handle  
    let contentLines = lines contents
    printShareName $ getShareName . head $ contentLines
    mapM_ putStrLn $ map show $ parseTransactions contents
    hClose handle

readDividends :: FilePath -> FilePath -> IO()
readDividends folder fileName = do
    putStrLn $ "reading dividend data from " ++ fileName
    let fullFileName = combine folder fileName
    handle <- openFile fullFileName ReadMode
    contents <- hGetContents handle  
    let contentLines = lines contents
    printShareName $ getShareName . head $ contentLines
    mapM_ putStrLn $ map show $ parseDividends contents
    hClose handle

