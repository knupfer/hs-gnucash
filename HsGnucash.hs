{-# LANGUAGE Arrows #-}

module Main where

import Data.List
import System.Environment
import Control.Arrow
import Text.XML.HXT.Core
import Data.Time
import Data.Maybe
import Data.Function

data Date = Date Integer Int Int deriving (Eq, Ord)
type Cent = Int
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
type AccountId = String
type Account = (AccountId, AccountType)
data Split = Split { getCent        :: Cent
                   , getAccountId   :: AccountId
                   , getAccountType :: AccountType} deriving (Show,Eq)
data Transaction = Transaction { getDay    :: Day
                               , getSplits :: [Split]
                               } deriving (Eq,Show)

main :: IO ()
main = do
  src:s:xs <- getArgs
  today    <- utctDay <$> getCurrentTime
  gnucash  <- readFile src
  let size = read s :: Integer
  [(accounts, transactions)] <- runX $ parseDoc gnucash
  putStr . toCsv
         . bin size today
         . (\x -> getProfit x ++ x)
         . filterAccounts xs
         . concatMap toSingleBook
         $ transactions

getProfit :: [Transaction] -> [Transaction]
getProfit ts = map (\(Transaction d [Split c i _]) -> Transaction d [Split c i Profit])
             . filterAccounts ["INCOME","EXPENSE"]
             $ concatMap toSingleBook ts

parseDoc ::  String -> IOSArrow XmlTree ([Account],[Transaction])
parseDoc doc = readString [withParseHTML yes, withWarnings no] doc
         >>> listA getAccounts &&& listA (getTransactions $< listA getAccounts)

toSingleBook :: Transaction -> [Transaction]
toSingleBook (Transaction day ss) = map (Transaction day . flip (:) []) ss

toCsv :: [Transaction] -> String
toCsv = (++) "Date\tMoney\tAccount\tId\n". concatMap
       (\(Transaction day (s:ss)) -> unlines $ f (show $ toFloatDate day) s
       : map (f "          ") ss)
       where f maybeDay x = intercalate "\t" [ maybeDay
                                             , show (getCent x)
                                             , show (getAccountType x)
                                             , show (getAccountId x) ]

toFloatDate :: Day -> Double
toFloatDate = f . toGregorian
  where f (y, m, d) = fromIntegral y
                      + ( fromIntegral (m-1)
                        + fromIntegral d / 30
                        ) / 12

toLedger :: Transaction -> String
toLedger _ = undefined

bin :: Integer -> Day -> [Transaction] -> [Transaction]
bin s today trans = concatMap (go (getDay $ head trans'))
    $ groupBy ((==) `on` (getAccountType . head . getSplits)) trans'
    where trans' = sortOn (getAccountType . head . getSplits &&& getDay)
                 $ concatMap toSingleBook trans
          go day (t:ts) = if today > addDays s day
             then Transaction day [ Split ( negate . sum
                                         . map (getCent . head . getSplits)
                                         . takeWhile ((>) (addDays s day) . getDay)
                                         $ dropWhile ((>) day . getDay) (t:ts))
                                   "NA" (getAccountType . head $ getSplits t)]
                  : go (addDays 1 day) (t:ts)
             else []
          go _ _ = []

filterAccounts :: [String] -> [Transaction] -> [Transaction]
filterAccounts xs = filter $ any ( flip elem (map toAccountType xs)
                                 . getAccountType)
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

getAccounts :: IOSArrow XmlTree Account
getAccounts = proc input -> do
            a  <- deepName "gnc:account" -< input
            i  <- deepText "act:id"      -< a
            t  <- deepText "act:type"    -< a
            t' <- arr toAccountType      -< t
            returnA                      -< (i, t')

deepName :: String -> IOSArrow XmlTree XmlTree
deepName = deep . hasName

deepText :: String -> IOSArrow XmlTree String
deepText s = deepName s >>> deep getText

getTransactions :: [Account] -> IOSArrow XmlTree Transaction
getTransactions accounts = proc input -> do
       t  <- deepName "gnc:transaction"       -< input
       d  <- deepName "trn:date-posted"       -< t
       s  <- deepName "trn:splits"            -< t
       d' <- deepText "ts:date"               -< d
       ms <- listA $ deepText "split:value"   -< s
       as <- listA $ deepText "split:account" -< s
       returnA -< Transaction (f d') (h ms as)
       where f x = fst . head $ reads x
             g x = fromJust $ lookup x accounts
             h xs ys = map (\(x,y) -> Split (f x) y (g y)) $ zip xs ys
