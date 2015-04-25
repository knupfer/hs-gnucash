{-# LANGUAGE Arrows #-}

module Main where

import Data.List
import System.Environment
import Control.Arrow
import Text.XML.HXT.Core
import Data.Time
import Data.Maybe
import Data.Function
import qualified Data.Map as M

data Cent = Cent Integer deriving (Eq)
instance Show Cent where
  show (Cent n) = show (n `div` 100) ++ "." ++ show (n `mod` 100)

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

type AccountId = String
data Account = Account { getAccountName :: String
                       , getType        :: AccountType
                       , getParent      :: Maybe Account
                       } deriving (Show, Eq)
type AccountM = M.Map AccountId Account
data Split = Split { getCent    :: Cent
                   , getAccount :: Account
                   } deriving (Show, Eq)
data Transaction = Transaction { getDay             :: Day
                               , getTransactionName :: String
                               , getSplits          :: [Split]
                               } deriving (Eq, Show)

main :: IO ()
main = do
  src:s:xs <- getArgs
  today    <- utctDay <$> getCurrentTime
  gnucash  <- readFile src
  let size = read s :: Integer
  [transactions] <- runX $ parseDoc gnucash
  putStr . toCsv
         . bin size today
         . (\x -> getProfit x ++ x)
         . filterAccounts xs
         . concatMap toSingleBook
         $ transactions

getProfit :: [Transaction] -> [Transaction]
getProfit ts = map (\(Transaction d _ [Split c _]) -> Transaction d "NA" [Split c (Account "NA" Profit Nothing)])
             . filterAccounts ["INCOME","EXPENSE"]
             $ concatMap toSingleBook ts

parseDoc ::  String -> IOSArrow XmlTree [Transaction]
parseDoc doc = readString [withParseHTML yes, withWarnings no] doc
         >>> listA (getTransactions $< getAccounts)

toSingleBook :: Transaction -> [Transaction]
toSingleBook (Transaction day n ss) = map (Transaction day n . flip (:) []) ss

toCsv :: [Transaction] -> String
toCsv = (++) "Date\tMoney\tName\tAccount\tId\n". concatMap
       (\(Transaction day n (s:ss)) -> unlines $ f n (show $ toFloatDate day) s
       : map (f n "          ") ss)
       where f n maybeDay x = intercalate "\t" [ maybeDay
                                               , show (getCent x)
                                               , n
                                               , show (getType $ getAccount x)
                                               , getAccountName $ getAccount x ]

toFloatDate :: Day -> Double
toFloatDate = f . toGregorian
  where f (y, m, d) = fromIntegral y
                      + ( fromIntegral (m-1)
                        + fromIntegral d / 30
                        ) / 12

toLedger :: Transaction -> String
toLedger (Transaction d n ss) = unlines $ [show d ++ " " ++ n]
             ++ map printSplit ss ++ [""]
  where getAH x = getAccountName x : maybe [] getAH (getParent x)
        printSplit (Split x y) = let t = "  " ++ intercalate ":" (getAH y) ++ "  "
                                 in t ++ replicate (80 - length (t ++ show x))
                                    ' ' ++ show x

bin :: Integer -> Day -> [Transaction] -> [Transaction]
bin s today trans = concatMap (go (getDay $ head trans'))
    $ groupBy ((==) `on` (getType . getAccount . head . getSplits)) trans'
    where trans' = sortOn (getType . getAccount . head . getSplits &&& getDay)
                 $ concatMap toSingleBook trans
          go day (t:ts) = if today > addDays s day
             then Transaction day "NA" [ Split (Cent . negate . sum
                                         . map ((\(Cent x) -> x). getCent . head . getSplits)
                                         . takeWhile ((>) (addDays s day) . getDay)
                                         $ dropWhile ((>) day . getDay) (t:ts))
                                   (Account "NA" (getType . getAccount . head $ getSplits t) Nothing)]
                  : go (addDays 1 day) (t:ts)
             else []
          go _ _ = []

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

getAccounts :: IOSArrow XmlTree AccountM
getAccounts = (>. ((\x -> M.map (f x) x) . M.fromList)) $ proc input -> do
            a  <- deepName "gnc:account" -< input
            i  <- deepText "act:id"      -< a
            t  <- deepText "act:type"    -< a
            p  <- deepText "act:parent"  -< a
            n  <- deepText "act:name"    -< a
            t' <- arr toAccountType      -< t
            returnA                      -< (i, (n, t', p))
            where f ls (x,y,z) = Account x y $ do
                                         (x',y',z') <- M.lookup z ls
                                         return $ f ls (x',y',z')

deepName :: String -> IOSArrow XmlTree XmlTree
deepName = deep . hasName

deepText :: String -> IOSArrow XmlTree String
deepText s = deepName s >>> deep getText

getTransactions :: AccountM -> IOSArrow XmlTree Transaction
getTransactions accounts = proc input -> do
       t  <- deepName "gnc:transaction"       -< input
       d  <- deepName "trn:date-posted"       -< t
       s  <- deepName "trn:splits"            -< t
       n  <- deepText "trn:description"       -< t
       d' <- deepText "ts:date"               -< d
       ms <- listA $ deepText "split:value"   -< s
       as <- listA $ deepText "split:account" -< s
       returnA -< Transaction (f d') n (h ms as)
       where f x = fst . head $ reads x
             g x = fromJust $ M.lookup x accounts
             h xs ys = map (\(x,y) -> Split (Cent $ f x) (g y)) $ zip xs ys
