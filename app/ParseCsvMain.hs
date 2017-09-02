module ParseCsvMain where

import Data.Char (isSpace)
import Data.Csv (FromRecord)
import Data.List
       (concatMap, dropWhile, dropWhileEnd, length, lines)
import Data.Map as M
       (Map, empty, findWithDefault, foldrWithKey, fromList, keys, toList,
        union)
import Hl.Csv.AccountSummary
       (AccountSummary, ShareHolding, date, findShareHolding,
        getAccountSummaries, holdingValue)
import Hl.Csv.Dividend (Dividend, amount, getDividends, paidOn)
import Hl.Csv.Transaction
       (Transaction(cost), actionedOn, getTransactions)
import Lib (createHoldings)
import ParseCsv (getShareName)
import System.Directory (getHomeDirectory, listDirectory)
import System.FilePath (combine)
import System.IO
       (BufferMode(LineBuffering), IOMode(ReadMode), hClose, hGetContents,
        hSetBuffering, openFile, putStrLn, stdout)
import Utils
       (defaultWhenNull, listFilesInFolder, stripWhitespace, toTwoDp)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  home <- getHomeDirectory
  let defaultAccountName = "isa"
  let accountPrompt = "Please provide an account name"
  accountName <- questionWithDefault accountPrompt defaultAccountName
  let baseFolder = home ++ "/Downloads/" ++ accountName
  let defaultDividendFolder = baseFolder ++ "/Dividends/"
  let dividendPrompt = "Pleade provide a dividends folder"
  dividendsFolder <- questionWithDefault dividendPrompt defaultDividendFolder
  dividendsMap <- getDividends dividendsFolder
  putStrLn $ showShareMap dividendsMap
  let defaultTransactionFolder = baseFolder ++ "/Transactions/"
  let transactionPrompt = "Please provide a transactions folder"
  transactionsFolder <-
    questionWithDefault transactionPrompt defaultTransactionFolder
  transactionsMap <- getTransactions transactionsFolder
  putStrLn $ showShareMap transactionsMap
  let defaultAccountSummaryFolder = baseFolder ++ "/AccountSummary"
  let accountSummaryPrompt = "Please provide an accountSummary folder"
  accountSummaryFolder <-
    questionWithDefault accountSummaryPrompt defaultAccountSummaryFolder
  accountSummaries <- getAccountSummaries accountSummaryFolder
  putStrLn $ concatMap ((++ "\n\n") . show) accountSummaries
  let shareTransactions = M.toList transactionsMap
  let divs = \s -> M.findWithDefault [] s dividendsMap
  let shareTransactionDividends =
        map (\(s, ts) -> (s, ts, divs s)) shareTransactions
  let as = head accountSummaries
  putStrLn "Share\t\t\t\t\t\t\tPriceProfit\tdividendProfit\ttotal"
  mapM_ (putStrLn . showProfit as) shareTransactionDividends
    --putStrLn $ concatMap ((++"\n\n") . show) shareTransactionDividends

-- set stdout to use linebuffering so that get/print IO actions work as expected
showProfit :: AccountSummary -> (String, [Transaction], [Dividend]) -> String
showProfit as (s, [], ds) = "\n"
showProfit as (s, t:ts, ds) =
  let mshareHolding = findShareHolding s as
  in case mshareHolding of
       Nothing -> "\n"
       Just sh ->
         s ++
         "\t" ++
         showR (priceProfit sh) ++
         "\t\t" ++ showR dividendProfit ++ "\t\t" ++ showR (totalProfit sh)
  where
    dividendProfit = transactionDividendProfit t as ds
    priceProfit sh = transactionPriceProfit t sh
    totalProfit sh = dividendProfit + priceProfit sh
    showR d = show (toTwoDp d)

-- | Calculate the profit from the transaction based purely on the share price change
transactionPriceProfit :: Transaction -> ShareHolding -> Double
transactionPriceProfit t s = holdingValue s - cost t

-- | Calculate the profit from the Dividends for the transaction.
transactionDividendProfit ::
     Transaction -> AccountSummary -> [Dividend] -> Double
transactionDividendProfit t as ds = sum $ filter inDateRange ds
  where
    sum = foldl (\v d -> v + amount d) 0
    inDateRange = (\d -> (paidOn d) > (actionedOn t) && (paidOn d) < (date as))

{-
 - for each transaction, get all the dividends for that transaction
 - then get the accountSummary/shareHOlding for the share - the name in the transaction is a prefix of the share name in the accountSummary
 - then using the shareholding and the transaction, calculate the gain/loss of the shareprice
 - then using the dividends and the date of the transaction/shareholding, calculate the dividends paid
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
  where
    entryToString share as acc = acc ++ shareTitle share ++ showValues as
    shareTitle share = "values for " ++ share ++ "\n"
    showValues as = concatMap ((++ "\n") . show) as

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
