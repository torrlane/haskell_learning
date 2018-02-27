module Hl.Csv.Account( Account(transactionsMap, dividendsMap, accountSummaries), FileContent, ShareName, loadAccount) where
import           Data.Char               (isSpace)
import           Data.Csv         (FromRecord)
import           Data.List               (dropWhileEnd)
import           Data.Map         as M (Map, empty, union)
import           Data.Text        (Text)
import           Data.Text.IO     as TIO (readFile)
import           Hl.Csv.Model     (AccountSummary, Dividend, ShareHolding,
                                   Transaction, parseAccountSummary,
                                   parseDividendsFromString,
                                   parseTransactionsFromString)
import           System.Directory (getHomeDirectory, listDirectory)
import           System.IO        (BufferMode (LineBuffering), hSetBuffering,
                                   putStrLn, stdout)
import           Utils            (FileContent, ShareName, defaultWhenNull,
                                   listFilesInFolder)

-- Data is loaded from 3 directories - /Dividends, /Transactions and /AccountSummary
-- Each file in the /Transaction folder should contain the sharename and that sharename should
-- match with the corresponding file in the /Dividends andAccountSummary folders.
--
-- The files in the AccountSummary folder represent the state of the portfolio at a particular
-- point in time (date is given in the file).
data Account = Account
  { transactionsMap  :: Map Text [Transaction]
  , dividendsMap     :: Map Text [Dividend]
  , accountSummaries :: [AccountSummary]
  } deriving (Show, Eq)

type Question = String

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

getDividendsMap :: FilePath -> IO (Map ShareName [Dividend])
getDividendsMap baseFolder = do
  let defaultFolder = baseFolder ++ "/Dividends/"
  let dividendPrompt = "Pleade provide a dividends folder"
  getShareMap dividendPrompt defaultFolder parseDividendsFromString

getTransactionsMap :: FilePath -> IO (Map ShareName [Transaction])
getTransactionsMap baseFolder = do
  let defaultFolder = baseFolder ++ "/Transactions/"
  let transactionPrompt = "Please provide a transactions folder"
  getShareMap transactionPrompt defaultFolder parseTransactionsFromString

getShareMap ::
     (FromRecord a)
  => Question
  -> FilePath
  -> (FileContent -> Map ShareName [a])
  -> IO (Map ShareName [a])
getShareMap question defaultFolder parseFromString = do
  folder <- questionWithDefault question defaultFolder
  files <- listFilesInFolder folder
  fileContents <- mapM TIO.readFile files
  return $ buildMap parseFromString fileContents

getAccountSummaries :: FilePath -> IO [AccountSummary]
getAccountSummaries baseFolder = do
  let defaultAccountSummaryFolder = baseFolder ++ "/AccountSummary"
  let accountSummaryPrompt = "Please provide an accountSummary folder"
  accountSummaryFolder <-
    questionWithDefault accountSummaryPrompt defaultAccountSummaryFolder
  accountSummaryFiles <- listFilesInFolder accountSummaryFolder
  accountSummaryContents <- mapM TIO.readFile accountSummaryFiles
  return $ map parseAccountSummary accountSummaryContents

{-
 - Takes a question to ask the user i.e "please provide a folder", and a default value.
 - Asks the user the question and returns their answer or the default if they answered with null.
 -}
questionWithDefault :: Question -> String -> IO String
questionWithDefault question dfault = do
  putStrLn $ question ++ " [" ++ dfault ++ "]"
  input <- getLine
  let answer = stripWhitespace $ defaultWhenNull dfault input
  return answer

stripWhitespace :: String -> String
stripWhitespace s = dropWhile isSpace $ dropWhileEnd isSpace s

-- Takes a parser function and a list of fileContents and produces a map from the share name to the lists of the parsed values
buildMap ::
     (FromRecord a)
  => (FileContent -> M.Map ShareName [a])
  -> [FileContent]
  -> M.Map ShareName [a]
buildMap parseContent = foldl acc M.empty
  where
    acc currentMap content = union currentMap $ parseContent content
