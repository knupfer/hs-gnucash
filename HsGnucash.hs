{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.Map                    as M
import           Data.Maybe
import qualified Data.Text                   as T
import           Data.Time
import           Filesystem.Path.CurrentOS   (decodeString)
import           System.Environment
import           Text.XML                    hiding (readFile)
import qualified Text.XML                    as X (readFile)
import           Text.XML.Cursor

data Cent = Cent Integer deriving (Eq)
instance Show Cent where
  show (Cent n) = show (n `div` 100) ++ "." ++ show (n `mod` 100)
                 ++ if 0 == n `mod` 10 then "0" else ""

data Date = Date Integer Int Int deriving (Eq, Ord)
data AccountType = Asset
                 | Bank
                 | Equity
                 | Expense
                 | Income
                 | Liability
                 | Payable
                 | Profit
                 | Receivable
                 deriving (Eq, Show, Ord)

type AccountId = T.Text
data Account = Account { getAccountName :: T.Text
                       , getType        :: AccountType
                       , getParent      :: Maybe Account
                       } deriving (Show, Eq)
type AccountM = M.Map AccountId Account
data Split = Split { getCent    :: Cent
                   , getAccount :: Account
                   } deriving (Show, Eq)
data Transaction = Transaction { getDay             :: Day
                               , getTransactionName :: T.Text
                               , getSplits          :: [Split]
                               } deriving (Eq, Show)

main :: IO ()
main = do
  src:t:s:xs <- getArgs
  today      <- utctDay      <$> getCurrentTime
  cursor     <- fromDocument <$> X.readFile def (decodeString src)
  putStr      $ output today (read t) (read s) cursor xs

output :: Day -> Int -> Integer -> Cursor -> [String] -> String
output today times size cursor xs = toCsv
  . map (\(Transaction d _ [Split (Cent c) a])
    -> Transaction d "NA" [Split (Cent $ -c*size) a])
  . foldl1 (.) (replicate times (bin size))
  . (\x -> getProfit x ++ x)
  . filterAccounts xs
  . noFuture today
  . concatMap toSingleBook
  $ getTransactions' cursor (getAccounts' cursor)

noFuture :: Day -> [Transaction] -> [Transaction]
noFuture d = filter ((>=) d . getDay)

getProfit :: [Transaction] -> [Transaction]
getProfit ts = map (\(Transaction d _ [Split c _]) -> Transaction d "NA" [Split c (Account "NA" Profit Nothing)])
             . filterAccounts ["INCOME","EXPENSE"]
             $ concatMap toSingleBook ts

toSingleBook :: Transaction -> [Transaction]
toSingleBook (Transaction day n ss) = map (Transaction day n . flip (:) []) ss

toCsv :: [Transaction] -> String
toCsv = concatMap
       (\(Transaction day n (s:ss)) -> unlines $ f n (show $ toFloatDate day) s
       : map (f n "          ") ss)
       where f n maybeDay x = intercalate "\t" [ maybeDay
                                               , show (getCent x)
                                               , T.unpack n
                                               , show (getType $ getAccount x)
                                               , T.unpack . getAccountName $ getAccount x ]

toFloatDate :: Day -> Double
toFloatDate = f . toGregorian
  where f (y, m, d) = fromIntegral y
                      + ( fromIntegral (m-1)
                        + fromIntegral (d-1) / 31
                        ) / 12

toLedger :: Transaction -> String
toLedger (Transaction d n ss) = unlines $ [show d ++ " " ++ T.unpack n]
             ++ map printSplit ss ++ [""]
  where getAH x = T.unpack (getAccountName x) : maybe [] getAH (getParent x)
        printSplit (Split x y) = let t = "  " ++ intercalate ":" (getAH y) ++ "  "
                                 in t ++ replicate (80 - length (t ++ show x))
                                    ' ' ++ show x

bin :: Integer -> [Transaction] -> [Transaction]
bin s trans = concatMap (\x -> filter ((>) (lastDay x) . addDays (s `div` 4) . getDay) $ go (firstDay x, lastDay x) x)
    $ groupBy ((==) `on` (getType . getAccount . head . getSplits)) trans'
    where trans' = sortOn (getType . getAccount . head . getSplits &&& getDay)
                 $ concatMap toSingleBook trans
          go (fDay, lDay) (t:ts) =  Transaction fDay "NA" [ Split (Cent . flip div (minimum [s, 1+diffDays lDay fDay]) . sum
                                         . map ((\(Cent x) -> x). getCent . head . getSplits)
                                         . takeWhile ((>) (addDays s fDay) . getDay)
                                         $ dropWhile ((>) fDay . getDay) (t:ts))
                                   (Account "NA" (getType . getAccount . head $ getSplits t) Nothing)]
                  : if fDay < lDay
                      then go (addDays 1 fDay, lDay) (t:ts)
                      else []
          go _ _ = []
          firstDay = getDay . head . sortOn getDay
          lastDay  = getDay . last . sortOn getDay

filterAccounts :: [String] -> [Transaction] -> [Transaction]
filterAccounts xs = filter $ any ( flip elem (map toAccountType xs)
                                 . getType
                                 . getAccount)
                           . getSplits

toAccountType :: String -> AccountType
toAccountType b = case b of
  "ASSET"      -> Asset
  "RECEIVABLE" -> Receivable
  "LIABILITY"  -> Liability
  "PAYABLE"    -> Payable
  "INCOME"     -> Income
  "EXPENSE"    -> Expense
  "EQUITY"     -> Equity
  "PROFIT"     -> Profit
  _            -> Bank

getAccounts' :: Cursor -> M.Map T.Text Account
getAccounts' c = ((\x -> M.map (f x) x) . M.fromList) $
                 filter (not . T.null .fst) $ c $// laxElement "account" >=> parseAccount''
  where f ls (x,y,z) = Account x y $ do
                         (x',y',z') <- M.lookup z ls
                         return $ f ls (x',y',z')

parseAccount'' :: Cursor -> [(T.Text, (T.Text, AccountType, T.Text))]
parseAccount'' c' = [(id' , (name', type', parent'))]
  where get n     = mconcat $ c' $/ laxElement n &/ content
        id'       = get "id"
        name'     = get "name"
        type'     = toAccountType . T.unpack $ get "type"
        parent'   = get "parent"

getTransactions' :: Cursor -> AccountM -> [Transaction]
getTransactions' c accounts = c $// laxElement "transaction" >=> parseTransaction'' accounts

parseTransaction'' :: M.Map T.Text Account -> Cursor -> [Transaction]
parseTransaction'' accounts c' = [Transaction (fst . head . reads $ date) desc h]
  where get n     = mconcat $ c' $/ laxElement n &/ content
        date      = T.unpack . mconcat $ c' $/ laxElement "date-posted" &/ laxElement "date" &/ content
        desc      = get "description"
        splits    = c' $/ laxElement "splits" &/ laxElement "split" >=> split
        split c'' = [(head . val &&& head . acc) c'']
        val i     = i $/ laxElement "value"   &/ content
        acc i     = i $/ laxElement "account" &/ content
        f x       = fst . head . reads $ T.unpack x
        g x       = fromJust $ M.lookup x accounts
        h         = map (\(x,y) -> Split (Cent $ f x) (g y)) splits
