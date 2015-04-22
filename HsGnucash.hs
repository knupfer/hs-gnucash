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
data Transaction = Transaction { getDay         :: Day
                               , getCent        :: Cent
                               , getAccountId   :: AccountId
                               , getAccountType :: AccountType
                               } deriving (Eq)

instance Show Transaction where
  show (Transaction d c aI aT) = unwords [ show . (\(yyyy,mm,dd) ->
                                         fromIntegral yyyy
                                         + (fromIntegral mm
                                           + fromIntegral dd / 30.5) / 12 :: Double)
                                         $ toGregorian d
                                     , show c, aI, show aT]

main :: IO ()
main = do
  src:s:xs <- getArgs
  today    <- utctDay <$> getCurrentTime
  gnucash  <- readFile src
  let size = read s
  let doc  = readString [withParseHTML yes, withWarnings no] gnucash
  accounts     <- runX $ doc >>> getAccounts
  transactions <- runX $ doc >>> getTransactions accounts
  putStrLn "Date Money Id Account"
  mapM_ print . bin size today . sortTransaction . filterAccounts xs $ getProfit transactions
  where sortTransaction = sortOn getAccountType . sortOn getDay
        getProfit xs    = xs ++ map (\(Transaction d c aI _) -> Transaction d c aI Profit)
                          (filterAccounts ["INCOME", "EXPENSE"] xs)

bin :: Integer -> Day -> [Transaction] -> [Transaction]
bin s today trans = concatMap (go (getDay $ head trans))
                $ groupBy ((==) `on` getAccountType) trans
   where go day (t:ts) = if today > addDays s day
            then Transaction day (negate . sum . map getCent
                 $ takeWhile ((>) (addDays s day) . getDay)
                 $ dropWhile ((>) day . getDay) ts) "NA" (getAccountType t)
                 : go (addDays 1 day) (t:ts)
            else []
         go _ _ = []

filterAccounts :: [String] -> [Transaction] -> [Transaction]
filterAccounts xs = filter $ flip elem (map toAccountType xs) . getAccountType

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
            returnA -<  (i, t')

deepName :: String -> IOSArrow XmlTree XmlTree
deepName = deep . hasName

deepText :: String -> IOSArrow XmlTree String
deepText s = deepName s >>> deep getText

getTransactions :: [Account] -> IOSArrow XmlTree Transaction
getTransactions accounts = proc input -> do
       t  <- deepName "gnc:transaction" -< input
       d  <- deepName "trn:date-posted" -< t
       s  <- deepName "trn:split"       -< t
       d' <- deepText "ts:date"         -< d
       m  <- deepText "split:value"     -< s
       a  <- deepText "split:account"   -< s
       returnA -<  Transaction (f d') (f m) a . fromJust $ lookup a accounts
       where f a = fst . head $ reads a

