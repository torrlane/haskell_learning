module ParseCsvMain where
import           Data.Char          (isSpace)
import           Data.Csv           (FromRecord)
import           Data.List          (dropWhile, dropWhileEnd, length, lines)
import           Data.Map           as M (Map, empty, fromList, keys, union, foldrWithKey)
import           Hl.Csv.Dividend    (Dividend, getDividends)
import           Hl.Csv.Transaction (getTransactions)
import           Lib                (createHoldings)
import           ParseCsv           (getShareName,
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
    dividendsMap <- getDividends dividendsFolder
    putStrLn $ printableDividends dividendsMap

    transactionsFolder <- requestFolder "Please provide a transactions folder" defaultTransactionFolder
    transactions <- getTransactions transactionsFolder

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
printableDividends :: M.Map String [Dividend] -> String
printableDividends ds = foldrWithKey entryToString "" ds
    where entryToString share ds acc = acc ++ (shareTitle share) ++ (dividends ds)  
          shareTitle share = "dividends for " ++ share ++ "\n"
          dividends ds = concat $ map (\d -> show d ++ "\n") ds

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

