{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Data.Function
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Time
import           Filesystem.Path.CurrentOS (decodeString)
import           System.Environment
import           Text.XML                  hiding (readFile)
import qualified Text.XML                  as X (readFile)
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

type AccountName = T.Text
type ParentName = T.Text
type AccountId = T.Text
data Account = Account { getAccountName :: AccountName
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
  . filterAccounts (map T.pack xs)
  . noFuture today
  . concatMap toSingleBook
  $ getTransactions cursor (getAccounts cursor)

noFuture :: Day -> [Transaction] -> [Transaction]
noFuture d = filter ((>=) d . getDay)

getProfit :: [Transaction] -> [Transaction]
getProfit ts = map (\(Transaction d _ [Split c _])
                    -> Transaction d "NA" [Split c (Account "NA" Profit Nothing)])
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
                                               , T.unpack . getAccountName $ getAccount x
                                               ]

toFloatDate :: Day -> Double
toFloatDate = f . toGregorian
  where f (y, m, d) = fromIntegral y
                      + ( fromIntegral (m-1)
                        + fromIntegral (d-1) / 31
                        ) / 12

toLedger :: Transaction -> T.Text
toLedger (Transaction d n ss) = T.unlines $ [T.pack (show d) <> " " <> n] <> map printSplit ss <> [""]
  where getAH x = getAccountName x : maybe [] getAH (getParent x)
        printSplit (Split x y) = let t = "  " <> T.intercalate ":" (getAH y) <> "  "
                                 in t <> T.pack (replicate (80 - T.length (t <> T.pack (show x))) ' ')
                                      <> T.pack (show x)

bin :: Integer -> [Transaction] -> [Transaction]
bin s trans = concatMap (\x -> filter ((>) (boundaryDay last x) . addDays (s `div` 4) . getDay)
                               $ bin' s (boundaryDay head x, boundaryDay last x) x)
                        $ groupBy ((==) `on` theirType) sortedTrans
     where sortedTrans   = sortOn (theirType &&& getDay) $ concatMap toSingleBook trans
           boundaryDay f = getDay . f . sortOn getDay

theirType :: Transaction -> AccountType
theirType = getType . getAccount . head . getSplits

bin' :: Integer -> (Day, Day) -> [Transaction] -> [Transaction]
bin' s (fDay, lDay) ts = Transaction fDay "NA"
  [ Split (Cent . flip div (minimum [s, 1+diffDays lDay fDay]) . sum
          . map ((\(Cent x) -> x) . getCent . head . getSplits)
          . takeWhile ((>) (addDays s fDay) . getDay)
          $ dropWhile ((>) fDay . getDay) ts
          )
  $ Account "NA" (theirType $ head ts) Nothing
  ] : if fDay < lDay
         then bin' s (addDays 1 fDay, lDay) ts
         else []
bin' _ _ _ = []

filterAccounts :: [T.Text] -> [Transaction] -> [Transaction]
filterAccounts xs = filter $ any ( flip elem (map toAccountType xs)
                                 . getType
                                 . getAccount
                                 ) . getSplits

toAccountType :: T.Text -> AccountType
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

getAccounts :: Cursor -> AccountM
getAccounts c = ((\x -> M.map (f x) x) . M.fromList) $
                filter (not . T.null .fst) $ c $// laxElement "account" >=> parseAccount
  where f ls (x,y,z) = Account x y $ do
                         (x',y',z') <- M.lookup z ls
                         return $ f ls (x',y',z')

parseAccount :: Cursor -> [(AccountId, (AccountName, AccountType, ParentName))]
parseAccount c' = [(id' , (name', type', parent'))]
  where get n   = mconcat $ c' $/ laxElement n &/ content
        id'     = get "id"
        name'   = get "name"
        type'   = toAccountType $ get "type"
        parent' = get "parent"

getTransactions :: Cursor -> AccountM -> [Transaction]
getTransactions c accounts = c $// laxElement "transaction" >=> parseTransaction accounts

parseTransaction :: AccountM -> Cursor -> [Transaction]
parseTransaction accounts c = [Transaction (fst . head . reads $ date) desc h]
  where date     = T.unpack . mconcat $ c $/ laxElement "date-posted" &/ laxElement "date" &/ content
        desc     = mconcat $ c $/ laxElement "description" &/ content
        splits   = c $/ laxElement "splits"  &/ laxElement "split" >=> split
        val i    = i $/ laxElement "value"   &/ content
        acc i    = i $/ laxElement "account" &/ content
        split c' = [(head . val &&& head . acc) c']
        f x      = fst . head . reads $ T.unpack x
        g x      = fromJust $ M.lookup x accounts
        h        = map (\(x,y) -> Split (Cent $ f x) (g y)) splits
