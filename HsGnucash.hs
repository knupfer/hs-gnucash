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
--  let size = read s
  [(accounts, transactions)] <- runX $ parseDoc gnucash
--  mapM_ print . bin size today . sortTransaction . filterAccounts xs $ getProfit transactions
  putStr . concatMap toCsv . sortTransaction . filterAccounts xs $ transactions
  where sortTransaction = sortOn getDay
        -- getProfit xs    = xs ++ map (\(Transaction d c aI _) -> Transaction d c aI Profit)
        --                   (filterAccounts ["INCOME", "EXPENSE"] xs)



parseDoc ::  String -> IOSArrow XmlTree ([Account],[Transaction])
parseDoc doc = readString [withParseHTML yes, withWarnings no] doc
         >>> listA getAccounts &&& listA (getTransactions $< listA getAccounts)

toCsv :: Transaction -> String
toCsv (Transaction day (s:ss)) = unlines $ f (show day) s:map (f "          ") ss
        where f maybeDay x = intercalate "\t" [ maybeDay
                                       , show (getCent x)
                                       , show (getAccountType x)
                                       , show (getAccountId x)]
toCsv _ = ""

toLedger :: Transaction -> String
toLedger (Transaction day (s:ss)) = unlines $ f (show day) s:map (f "          ") ss
        where f maybeDay x = intercalate "\t" [ maybeDay
                                       , show (getCent x)
                                       , show (getAccountType x)
                                       , show (getAccountId x)]

-- bin :: Integer -> Day -> [Transaction] -> [Transaction]
-- bin s today trans = concatMap (go (getDay $ head trans))
--                 $ groupBy ((==) `on` getAccountType) trans
--    where go day (t:ts) = if today > addDays s day
--             then Transaction day (negate . sum . map getCent
--                  $ takeWhile ((>) (addDays s day) . getDay)
--                  $ dropWhile ((>) day . getDay) ts) "NA" (getAccountType t)
--                  : go (addDays 1 day) (t:ts)
--             else []
--          go _ _ = []

filterAccounts :: [String] -> [Transaction] -> [Transaction]
filterAccounts xs = filter $ any (flip elem (map toAccountType xs) . getAccountType) . getSplits

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
       t  <- deepName "gnc:transaction" -< input
       d  <- deepName "trn:date-posted" -< t
       s  <- deepName "trn:splits"      -< t
       d' <- deepText "ts:date"         -< d
       m  <- listA $ deepText "split:value"   -< s
       a  <- listA $ deepText "split:account" -< s
       xs <- arr (uncurry zip) -< (m, a)
       xs'<- arr (map (\(m, a) -> Split (f m) a (g a))) -< xs
       returnA -< Transaction (f d') xs'
       where f x = fst . head $ reads x
             g x = fromJust $ lookup x accounts
