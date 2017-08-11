module ParseCsvMain where
import           Data.Char          (isSpace)
import           Data.Csv           (FromRecord)
import           Data.List          (dropWhile, dropWhileEnd, length, lines)
import           Data.Map           as M (Map, empty, fromList, keys, union, foldrWithKey)
import           Hl.Csv.Dividend    (Dividend, getDividends)
import           Hl.Csv.Transaction (Transaction, getTransactions)
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
    let dividendPrompt = "Pleade provide a dividends folder"
    let defaultTransactionFolder = home ++ "/Downloads/Transactions/"
    let transactionPrompt = "Please provide a transactions folder"
    let defaultAccountSummaryFolder = home ++ "/Downloads/AccountSummary"
    let accountSummaryPrompt = "Please provide an accountSummary folder"

    dividendsFolder <- questionWithDefault dividendPrompt defaultDividendFolder
    dividendsMap <- getDividends dividendsFolder
    putStrLn $ showShareMap dividendsMap

    transactionsFolder <- questionWithDefault transactionPrompt defaultTransactionFolder
    transactionsMap <- getTransactions transactionsFolder
    putStrLn $ showShareMap transactionsMap

    accountSummaryFolder <- questionWithDefault accountSummaryPrompt defaultAccountSummaryFolder
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

-- | returns a string representation of the map
showShareMap :: (Show a) => M.Map String [a] -> String
showShareMap as = foldrWithKey entryToString "" as
    where entryToString share as acc = acc ++ (shareTitle share) ++ (showValues as)  
          shareTitle share = "values for " ++ share ++ "\n"
          showValues as = concat $ map (\a -> show a ++ "\n") as

{-
 - Takes a question to ask the user i.e "please provide a folder", and a default value.
 - Asks the user the question an returns their answer or the default if they answered with null.
 -}
questionWithDefault :: String -> String -> IO String
questionWithDefault question dfault = do
    putStrLn $ question ++ " [" ++ dfault ++ "]"
    input <- getLine
    let answer = stripWhitespace $ defaultWhenNull dfault input
    return answer

