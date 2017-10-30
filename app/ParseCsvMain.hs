module ParseCsvMain where

import           Data.Char             (isSpace)
import           Data.Csv              (FromRecord)
import           Data.List             (concatMap, dropWhile, dropWhileEnd,
                                        filter, length, lines)
import           Data.Map              as M (Map, empty, findWithDefault,
                                             foldrWithKey, fromList, keys,
                                             toList, union)
import           Data.Time.Calendar    (showGregorian)
import           Hl.Csv.Model          (AccountSummary, Dividend, ShareHolding,
                                        Transaction (cost, sharesBought),
                                        actionedOn, amount, date,
                                        findShareHolding, holdingValue, paidOn,
                                        parseAccountSummary,
                                        parseDividendsFromString,
                                        parseTransactionsFromString, unitsHeld)
import           Lib                   (createHoldings)
import           ParseCsv              (buildMap, getShareName)
import           System.Directory      (getHomeDirectory, listDirectory)
import           System.FilePath       (combine)
import           System.IO             (BufferMode (LineBuffering),
                                        IOMode (ReadMode), hClose, hGetContents,
                                        hSetBuffering, openFile, putStrLn,
                                        stdout)
import           Text.Tabular          as T (Table, col, empty, row, (+----+),
                                             (+.+), (^..^), (^|^), (^||^))
import           Text.Tabular.AsciiArt (render)
import           Utils                 (defaultWhenNull, listFilesInFolder,
                                        stripWhitespace, toTwoDp)

getBaseFolder :: IO FilePath
getBaseFolder = do
  let defaultAccountName = "isa"
  let accountPrompt = "Please provide an account name"
  accountName <- questionWithDefault accountPrompt defaultAccountName
  home <- getHomeDirectory
  return $ home ++ "/Downloads/" ++ accountName

getDividendsMap :: FilePath -> IO (Map String [Dividend])
getDividendsMap baseFolder = do
  let defaultFolder = baseFolder ++ "/Dividends/"
  let dividendPrompt = "Pleade provide a dividends folder"
  getShareMap dividendPrompt defaultFolder parseDividendsFromString

getTransactionsMap :: FilePath -> IO (Map String [Transaction])
getTransactionsMap baseFolder = do
  let defaultFolder = baseFolder ++ "/Transactions/"
  let transactionPrompt = "Please provide a transactions folder"
  getShareMap transactionPrompt defaultFolder parseTransactionsFromString

getShareMap ::
     (FromRecord a)
  => String
  -> FilePath
  -> (String -> Map String [a])
  -> IO (Map String [a])
getShareMap prompt defaultFolder parseFromString = do
  folder <- questionWithDefault prompt defaultFolder
  files <- listFilesInFolder folder
  fileContents <- mapM readFile files
  let map = buildMap parseFromString fileContents
  return map

getAccountSummaries :: FilePath -> IO [AccountSummary]
getAccountSummaries baseFolder = do
  let defaultAccountSummaryFolder = baseFolder ++ "/AccountSummary"
  let accountSummaryPrompt = "Please provide an accountSummary folder"
  accountSummaryFolder <-
    questionWithDefault accountSummaryPrompt defaultAccountSummaryFolder
  accountSummaryFiles <- listFilesInFolder accountSummaryFolder
  accountSummaryContents <- mapM readFile accountSummaryFiles
  return $ map parseAccountSummary accountSummaryContents

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  baseFolder <- getBaseFolder
  dividendsMap <- getDividendsMap baseFolder
  putStrLn $ showShareMap dividendsMap
  transactionsMap <- getTransactionsMap baseFolder
  putStrLn $ showShareMap transactionsMap
  accountSummaries <- getAccountSummaries baseFolder
  putStrLn $ concatMap ((++ "\n\n") . show) accountSummaries
  let shareTransactions = M.toList transactionsMap
  let divs s = M.findWithDefault [] s dividendsMap
  let shareTransactionDividends =
        map (\(s, ts) -> (s, filter (\t -> fromIntegral (sharesBought t) /= 0 ) ts, divs s)) shareTransactions
  let as = last accountSummaries
  let accountSummaryDate = date as
  putStrLn $ "data from: " ++ showGregorian accountSummaryDate
  let tableHeaders = T.empty ^..^ col "Price Profit" [] ^|^ col "Dividend Profit" [] ^|^ col "Total Profit" []
  let tableData = foldl (tabD as) tableHeaders shareTransactionDividends
  putStrLn $ render id id id tableData

tabD :: AccountSummary -> Table String ch String -> (String, [Transaction], [Dividend]) -> Table String ch String
tabD as table (s, [], ds) = table
tabD as table (s, t:ts, ds) =
  let mshareHolding = findShareHolding s as
  in case mshareHolding of
      Nothing -> table
      Just sh -> table +.+ row s [ showR (priceProfit sh), showR dividendProfit, showR (totalProfit sh)]
  where
    dividendProfit = transactionDividendProfit t as ds
    priceProfit = transactionPriceProfit t
    totalProfit sh = dividendProfit + priceProfit sh
    showR d = show (toTwoDp d)

-- | Calculate the profit from the transaction based purely on the share price change
transactionPriceProfit :: Transaction -> ShareHolding -> Double
transactionPriceProfit t s =
  let numBought = fromInteger $ fromIntegral $ sharesBought t
      costPerShare = cost t / numBought
      pricePerShare = holdingValue s / unitsHeld s
      profitPerShare = pricePerShare - costPerShare
  in
  numBought * profitPerShare

-- | Calculate the profit from the Dividends for the transaction.
transactionDividendProfit ::
     Transaction -> AccountSummary -> [Dividend] -> Double
transactionDividendProfit t as ds = sum $ filter inDateRange ds
  where
    sum = foldl (\v d -> v + divAmount t d) 0
    divAmount t d = fromIntegral (sharesBought t) * amount d / 100
    inDateRange d = paidOn d > actionedOn t && paidOn d < date as

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
showShareMap = foldrWithKey entryToString ""
  where
    entryToString share as acc = acc ++ shareTitle share ++ showValues as
    shareTitle share = "values for " ++ share ++ "\n"
    showValues = concatMap ((++ "\n") . show)

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
