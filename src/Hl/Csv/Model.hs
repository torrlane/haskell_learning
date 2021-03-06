{-# LANGUAGE OverloadedStrings #-}

module Hl.Csv.Model
  ( Transaction(Transaction, actionedOn, sharesBought, cost)
  , Dividend(Dividend, paidOn, amount)
  , AccountSummary(date)
  , ShareHolding(ShareHolding, shareName, unitsHeld, sharePrice)
  , dividendsPaidUpto
  , dividendProfit
  , findShareHolding
  , getShareName
  , numberHeld
  , parseAccountSummary
  , parseShareHoldings
  , parseDividendsFromString
  , parseTransactionsFromString
  , priceProfit
  , totalProfit
  ) where

import Control.Monad (mzero)
import Data.Csv
  ( FromRecord(parseRecord)
  , HasHeader(NoHeader)
  , Parser
  , (.!)
  , decode
  , toField
  )
import Data.Either (fromRight)
import Data.List (drop, find, isPrefixOf, length, null, take, zip)
import Data.Map as M (Map, empty, fromList)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
  ( append
  , drop
  , dropWhile
  , head
  , isPrefixOf
  , length
  , lines
  , null
  , tail
  , take
  , takeWhile
  , unlines
  )
import Data.Time.Calendar (Day(..))
import Data.Vector as V (Vector, empty, toList)
import Utils
  ( FileContent
  , ShareName
  , (~=)
  , convertToLazyByteString
  , listFilesInFolder
  , parseDate
  , parseDateWithFormat
  , parseDouble
  , parseInt
  , stripDoubleQuotes
  , stripTextWhitespace
  , toFiveDp
  )

{-
 - cost is the total cost of the Transaction, not the individual cost per unit
 - -}
data Transaction = Transaction
  { actionedOn :: Day
  , sharesBought :: Int
  , cost :: Double
  } deriving (Read, Show, Eq)

data AccountSummary = AccountSummary
  { date :: Day
  , shareHoldings :: [ShareHolding]
  } deriving (Show, Eq)

-- | contains details of how many of a particular share is held at a certain point in time, and the shareprice(in pence) at that time
data ShareHolding = ShareHolding
  { shareName :: Text
  , unitsHeld :: Double
  , sharePrice :: Double
  } deriving (Show, Eq)

data Dividend = Dividend
  { paidOn :: Day
  , amount :: Double
  } deriving (Read, Show)

-- equals doesn't work well for Doubles. Make Dividend an instance of Eq and use a small error when comparing the Dividend amounts
instance Eq Dividend where
  a == b = paidOn a == paidOn b && amount a ~= amount b

-- | Calculate the profit from the transaction (in pounds) based purely on the share price change
priceProfit :: Transaction -> ShareHolding -> Double
priceProfit t s =
  let numBought = fromInteger $ fromIntegral $ sharesBought t
      costPerShare = cost t / numBought
      pricePerShare = holdingValue s / unitsHeld s
      profitPerShare = pricePerShare - costPerShare
   in numBought * profitPerShare

-- | The total profit from the transaction (dividends and price profit) up to end date
totalProfit :: Transaction -> ShareHolding -> [Dividend] -> Day -> Double
totalProfit t sh ds end = dividendProfit t end ds + priceProfit t sh

-- | Calculate the profit from the Dividends (up to end date) for the transaction (in pounds).
dividendProfit :: Transaction -> Day -> [Dividend] -> Double
dividendProfit t end ds = sum $ filter inDateRange ds
  where
    sum = foldl (\v d -> v + divAmount t d) 0
    divAmount t d = fromIntegral (sharesBought t) * amount d / 100
    inDateRange d = paidOn d > actionedOn t && paidOn d < end

{- Calculates the amount of dividends paid up to the specified date (inclusive) -}
dividendsPaidUpto :: Day -> [Dividend] -> [Transaction] -> Double
dividendsPaidUpto d ds ts = sum $ map (dividend_amount d) ds
  where
    dividend_amount day (Dividend paid_on amount)
      | paid_on > day = 0
      | otherwise = fromIntegral (numberHeld day ts) * amount

parseTransactions :: FileContent -> [Transaction]
parseTransactions str =
  toList $ fromRight V.empty $ decodeCsv transactionHeader str

{- takes a csv file and returns a map from the sharename to a list of dividends/transactions/... from the file
 -}
parseTransactionsFromString :: FileContent -> M.Map ShareName [Transaction]
parseTransactionsFromString contents = mapMaybe shareName values
  where
    contentLines = T.lines contents
    shareName = getShareName . head $ contentLines
    values = parseTransactions contents
    mapMaybe Nothing v = M.empty
    mapMaybe (Just k) v = M.fromList [(k, v)]

transactionHeader :: Int
transactionHeader = 9

{- the round is to convert from a Double i.e "123.00" to an Integer. Unfortunately, to get it to work, it needs to cast the input to a Double.
 -
 - This code is using Applicative Functor style. The <$> functions take a function from a -> b and an instance of Applicative Functor f a (say) and returns f b. Now in this case, it b is a function, so the <*>'s provide arguments to b, but wrapped in the Applicative f. It's not as complicated as it sounds!
 -}
instance FromRecord Transaction where
  parseRecord v
    | length v >= 6 =
      Transaction <$> (parseDate <$> v .! 0) <*>
      (round <$> (v .! 4 :: Parser Double)) <*>
      v .! 5
    | otherwise = mzero

{- the number of shares held on the specified date (inclusive)-}
numberHeld :: Day -> [Transaction] -> Int
numberHeld day ts = sum $ map calculateChangeToHolding ts
  where
    calculateChangeToHolding (Transaction transaction_date sharesBought _)
      | transaction_date > day = 0
      | otherwise = sharesBought

parseDividends :: FileContent -> [Dividend]
parseDividends str = V.toList $ fromRight V.empty $ decodeCsv dividendHeader str

{- takes a csv String and returns a map from the sharename to a list of dividends/transactions/... from the file
 -}
parseDividendsFromString :: FileContent -> M.Map ShareName [Dividend]
parseDividendsFromString contents = mapMaybe shareName values
  where
    shareName = getShareName . head . T.lines $ contents
    values = parseDividends contents
    mapMaybe Nothing v = M.empty
    mapMaybe (Just k) v = M.fromList [(k, v)]

dividendHeader :: Int
dividendHeader = 9

{- The zeroth field contains the date that the dividend was paid on, the 4th field the number of shares held, and the 5th field the total amount paid. We divide the total amount by the number of shares, to get a dividend amount per share (in pence, to 5 dp)
 -}
instance FromRecord Dividend where
  parseRecord v
    | length v >= 6 =
      Dividend <$> (parseDate <$> v .! 0) <*>
      (toFiveDp . (100 *) <$>
       ((/) <$> (v .! 5 :: Parser Double) <*> (v .! 4 :: Parser Double)))
    | otherwise = mzero

-- | the total value of the holding (in pounds) e.g. sharePrice * unitsHeld / 100
holdingValue :: ShareHolding -> Double
holdingValue ShareHolding {unitsHeld = h, sharePrice = p} = (h * p) / 100

-- | takes a name of a share and an AccountSummary and returns the first ShareHolding
-- where the name of the share in the ShareHolding is a prefix of the input String.
-- This is because the HL AccountSummary adds stuff like "25 *1" to the shareName.
findShareHolding :: Text -> AccountSummary -> Maybe ShareHolding
findShareHolding share AccountSummary {shareHoldings = shs} =
  find (T.isPrefixOf share . shareName) shs

parseAccountSummary :: FileContent -> AccountSummary
parseAccountSummary accountSummaryContents =
  AccountSummary {date = asDate, shareHoldings = holdings}
  where
    asDate = getAccountSummaryDate accountSummaryContents
    holdings = parseShareHoldings accountSummaryContents

-- | takes the csv content of the accountSummary and extracts the date
-- TODO - this returns the epoch date if there are problems with parsing. It should return an Either.
getAccountSummaryDate :: FileContent -> Day
getAccountSummaryDate content = parseDateWithFormat "%d-%m-%Y" unparsed
  where
    prefix = "Spreadsheet created at,"
    mapMaybe Nothing = T.append prefix "01-01-1970"
    mapMaybe (Just s) = s
    line = mapMaybe $ find (T.isPrefixOf prefix) (T.lines content)
    unparsed = unpack $ T.take 10 $ T.drop (T.length prefix) line

shareHoldingHeader :: Int
shareHoldingHeader = 11

parseShareHoldings :: FileContent -> [ShareHolding]
parseShareHoldings str =
  V.toList $ fromRight V.empty $ decodeCsv shareHoldingHeader str

instance FromRecord ShareHolding where
  parseRecord v
    | length v >= 3 =
      ShareHolding <$> strip (v .! 0) <*> (parseDouble <$> strip (v .! 1)) <*>
      (parseDouble <$> strip (v .! 2))
    | otherwise = mzero

strip :: Parser Text -> Parser Text
strip p = (pack . stripDoubleQuotes . unpack) <$> p

-- Takes a String, removes any header lines, and then parses the remaining lines into instances of Type a
decodeCsv :: FromRecord a => Int -> FileContent -> Either String (Vector a)
decodeCsv h str =
  decode NoHeader $
  convertToLazyByteString . toField . stripHeader h . stripFooter $ str

-- Removes the header section from the transactions csv file, returning just the csv lines
stripHeader :: Int -> FileContent -> FileContent
stripHeader h = T.unlines . drop h . T.lines

stripFooter :: FileContent -> FileContent
stripFooter =
  T.unlines . takeWhile (not . T.isPrefixOf (pack "\"Totals\"")) . T.lines

getShareName :: FileContent -> Maybe ShareName
getShareName s
  | T.null (shareName s) = Nothing
  | otherwise = Just $ shareName s
  where
    shareName =
      stripTextWhitespace . T.takeWhile (',' /=) . T.tail . T.dropWhile (',' /=)
