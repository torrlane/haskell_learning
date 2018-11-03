module ParseCsvMain where

import Data.List (filter)
import Data.Map as M (Map, findWithDefault, toList)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Time.Calendar (diffDays, showGregorian)
import Hl.Csv.Account
  ( Account(accountSummaries, dividendsMap, transactionsMap)
  , loadAccount
  )
import Hl.Csv.Model
  ( AccountSummary
  , Dividend
  , ShareHolding
  , Transaction(cost, sharesBought)
  , actionedOn
  , date
  , dividendProfit
  , findShareHolding
  , priceProfit
  )
import System.IO (putStrLn)
import Text.Tabular as T (Table, (+.+), (^..^), (^|^), col, empty, row)
import Text.Tabular.AsciiArt (render)
import Utils (toTwoDp)

main :: IO ()
main = do
  account <- loadAccount
  let shareTransactions = M.toList $ transactionsMap account
  let divs s = M.findWithDefault [] s $ dividendsMap account
  -- shareTransactionDividends is a list of tuples (Share, [purchases], [dividends])
  let shareTransactionDividends =
        map
          (\(s, ts) ->
             (s, filter (\t -> fromIntegral (sharesBought t) /= 0) ts, divs s))
          shareTransactions
  let as = last $ accountSummaries account
  let accountSummaryDate = date as
  putStrLn $ "data from: " ++ showGregorian accountSummaryDate
  let cols =
        [ transactionDateColumn
        , costColumn
        , priceProfitColumn as
        , dividendProfitColumn as
        , totalProfitColumn as
        , totalPercentProfitColumn as
        , annualisedPercentProfitColumn as
        ]
  let tabData = createTable cols account shareTransactionDividends
  putStrLn $ render id id id tabData

type ShareName = Text

type ColumnValue = (ShareName, Transaction, [Dividend]) -> String

type TableColumn = (String, ColumnValue)

columnName = fst

columnValue = snd

transactionDateColumn = ("Purchased on", \(_, t, _) -> (show . actionedOn) t)

costColumn = ("Cost", \(_, t, _) -> (show . cost) t)

priceProfitColumn as = ("Price Profit", toTwoDpF transactionPriceProfit as)

dividendProfitColumn as =
  ("Dividend Profit", toTwoDpF transactionDividendProfit as)

totalProfitColumn as = ("Total Profit", toTwoDpF totalProfit as)

totalPercentProfitColumn as = ("Total % Profit", toTwoDpF totalPercentProfit as)

annualisedPercentProfitColumn as =
  ("Annual % Profit", toTwoDpF annualisedPercentProfit as)

transactionPriceProfit as (s, t, _) =
  priceProfit t $ fromJust $ findShareHolding s as

transactionDividendProfit as (_, t, ds) = dividendProfit t (date as) ds

totalProfit as x = transactionDividendProfit as x + transactionPriceProfit as x

totalPercentProfit as x@(_, t, _) = 100 * totalProfit as x / cost t

annualisedPercentProfit as x@(_, t, _) =
  (\d -> 100 * (d - 1)) $
  flip (**) (1 / yearsHeld as t) $ (cost t + totalProfit as x) / cost t

daysHeld as t = diffDays (date as) (actionedOn t)

yearsHeld as t = fromIntegral (daysHeld as t) / (365 :: Double)

-- Take a function that returns a double, execute it, and format the result to 2 dp.
toTwoDpF f as x = (show . toTwoDp) (f as x)

createTable ::
     [TableColumn]
  -> Account
  -> [(ShareName, [Transaction], [Dividend])]
  -> Table String String String
createTable cols account shareTransactionDividends
  -- headers is actually a table. There is a leftmost column that isn't described here.
  -- The library makes it difficult not to have that column. In this case, we'll use it for the name of the share/transaction
 = do
  let hCols = map (\c -> col (columnName c) []) cols
  let headers = foldl (^|^) (T.empty ^..^ head hCols) (tail hCols)
  foldl (createRow cols) headers shareTransactionDividends
  where
    createRow ::
         [TableColumn]
      -> Table String ch String
      -> (ShareName, [Transaction], [Dividend])
      -> Table String ch String
    createRow cols table (s, ts, ds) =
      foldl (\tab t -> tab +.+ row (unpack s) (values t)) table ts
      where
        values t = map (\c -> columnValue c (s, t, ds)) cols
