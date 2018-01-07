module ParseCsvMain where

import           Data.List             (filter)
import           Data.Map              as M (Map, findWithDefault, toList)
import           Data.Time.Calendar    (diffDays, showGregorian)
import           Hl.Csv.Account        (Account (accountSummaries, dividendsMap, transactionsMap),
                                        loadAccount)
import           Hl.Csv.Model          (AccountSummary, Dividend, ShareHolding,
                                        Transaction (cost, sharesBought),
                                        actionedOn, amount, date,
                                        findShareHolding, holdingValue, paidOn,
                                        totalProfit, transactionDividendProfit,
                                        transactionPriceProfit, unitsHeld)
import           System.IO             (putStrLn)
import           Text.Tabular          as T (Table, col, empty, row, (+----+),
                                             (+.+), (^..^), (^|^), (^||^))
import           Text.Tabular.AsciiArt (render)
import           Utils                 (defaultWhenNull, stripWhitespace,
                                        toTwoDp)

main :: IO ()
main = do
  account <- loadAccount
  let shareTransactions = M.toList $ transactionsMap account
  let divs s = M.findWithDefault [] s $ dividendsMap account
  -- shareTransactionDividends is a list of tuples (Share, [purchases], [transactions])
  let shareTransactionDividends =
        map (\(s, ts) -> (s, filter (\t -> fromIntegral (sharesBought t) /= 0 ) ts, divs s)) shareTransactions
  let as = last $ accountSummaries account
  let accountSummaryDate = date as
  putStrLn $ "data from: " ++ showGregorian accountSummaryDate
  -- tableHeaders is actually a table. There is a leftmost column that isn't described here.
  -- The library makes it difficult not to have that column. In this case, we'll use it for the name of the share/transaction
  let tableHeaders = T.empty ^..^ col "Transaction Date" [] ^|^ col "Cost" [] ^|^ col "Price Profit" [] ^|^ col "Dividend Profit" [] ^|^ col "Total Profit" [] ^|^ col "Total % Profit" [] ^|^ col "Annualised %" []
  -- use the tabD function to append rows to the tableHeaders "table".
  let tableData = foldl (tabD as) tableHeaders shareTransactionDividends
  putStrLn $ render id id id tableData

type ShareName = String

-- Takes a table and the data about a particular share and appends rows to the table. One row for each Transaction
tabD :: AccountSummary -> Table String ch String -> (ShareName, [Transaction], [Dividend]) -> Table String ch String
tabD as table (s, [], ds) = table
tabD as table (s, t:ts, ds) =
  let mshareHolding = findShareHolding s as
  in case mshareHolding of
      Nothing -> table
      Just sh -> foldl (\tab t -> tab +.+ createRow s sh t) table (t:ts)
  where
    dividendProfit t = transactionDividendProfit t (date as) ds
    priceProfit = transactionPriceProfit
    tp sh = totalProfit t sh ds (date as)
    showR d = show (toTwoDp d)
    daysHeld t = diffDays (date as) (actionedOn t)
    yearsHeld t = fromIntegral (daysHeld t) / (365::Double)
    annualisedPercent t sh = (\d -> 100 * (d-1)) $ flip (**) (1/yearsHeld t) $ (cost t + tp sh) / cost t
    createRow s sh t = row s [ (show . actionedOn) t, (show . cost) t, showR (priceProfit t sh), showR (dividendProfit t), showR (tp sh), showR (100 * tp sh / cost t), showR (annualisedPercent t sh)]

