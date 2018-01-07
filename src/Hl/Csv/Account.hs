module Hl.Csv.Account( Account(transactionsMap, dividendsMap, accountSummaries), loadAccount) where
import           Data.Csv         (FromRecord)
import           Data.Map         as M (Map)
import           Hl.Csv.Model     (AccountSummary, Dividend, ShareHolding,
                                   Transaction, parseAccountSummary,
                                   parseDividendsFromString,
                                   parseTransactionsFromString)
import           ParseCsv         (buildMap)
import           System.Directory (getHomeDirectory, listDirectory)
import           System.IO        (BufferMode (LineBuffering), hSetBuffering,
                                   putStrLn, stdout)
import           Utils            (defaultWhenNull, listFilesInFolder,
                                   stripWhitespace)

data Account = Account
  { transactionsMap  :: Map String [Transaction]
  , dividendsMap     :: Map String [Dividend]
  , accountSummaries :: [AccountSummary]
  } deriving (Show, Eq)

loadAccount :: IO Account
loadAccount = do
  hSetBuffering stdout LineBuffering
  baseFolder <- getBaseFolder
  dividendsMap <- getDividendsMap baseFolder
  transactionsMap <- getTransactionsMap baseFolder
  accountSummaries <- getAccountSummaries baseFolder
  return Account{transactionsMap=transactionsMap, dividendsMap=dividendsMap, accountSummaries=accountSummaries}

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
