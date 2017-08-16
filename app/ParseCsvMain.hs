module ParseCsvMain where
import           Data.Char          (isSpace)
import           Data.Csv           (FromRecord)
import           Data.List          (concatMap, dropWhile, dropWhileEnd, length, lines)
import           Data.Map           as M (Map, empty, fromList, keys, union, foldrWithKey)
import           Hl.Csv.Dividend    (Dividend, getDividends)
import           Hl.Csv.Transaction (Transaction, getTransactions)
import           Hl.Csv.AccountSummary (getAccountSummaries)
import           Lib                (createHoldings)
import           ParseCsv           (getShareName)
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
    accountSummaries <- getAccountSummaries accountSummaryFolder
    putStrLn $ concatMap ((++"\n\n") . show) accountSummaries

{-
 - accountSummaries provide share values i.e. how much a share is worth at a particular point in time.
 - calculate the total amount of dividends paid for a transaction
 -
 - dividendsPaid :: Transaction -> [Dividend] -> amount // let's not worry about share sales.
 - sharePriceProfit :: Transaction -> AccountSummary -> amount
 - calculate the total profit on the shareprice for a transaction (using the latest accountSummary to find the end price)
 -}

-- | returns a string representation of a map from Share to an array of type 'a'
showShareMap :: (Show a) => M.Map String [a] -> String
showShareMap as = foldrWithKey entryToString "" as
    where entryToString share as acc = acc ++ (shareTitle share) ++ (showValues as)  
          shareTitle share = "values for " ++ share ++ "\n"
          showValues as = concatMap ((++"\n") . show) as

{-
 - Takes a question to ask the user i.e "please provide a folder", and a default value.
 - Asks the user the question and returns their answer or the default if they answered with null.
 -}
questionWithDefault :: String -> String -> IO String
questionWithDefault question dfault = do
    putStrLn $ question ++ " [" ++ dfault ++ "]"
    input <- getLine
    let answer = stripWhitespace $ defaultWhenNull dfault input
    return answer

