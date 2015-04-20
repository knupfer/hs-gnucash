{-# LANGUAGE Arrows #-}

module Main where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
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
  src:xs  <- getArgs
  day <- utctDay <$> getCurrentTime
  gnucash <- readFile src
  let doc = readString [withParseHTML yes, withWarnings no] gnucash
  accounts     <- runX $ doc >>> getAccounts
  transactions <- runX $ doc >>> getTransactions accounts
  putStrLn "Date Money Id Account"
  mapM_ print . bin day . sortTransaction . filterAccounts xs $ getProfit transactions
  where sortTransaction = sortOn getAccountType . sortOn getDay
        getProfit xs = xs ++ map (\(Transaction d c aI _) -> Transaction d c aI Profit)
                       (filterAccounts ["INCOME", "EXPENSE"] xs)

bin :: Day -> [Transaction] -> [Transaction]
bin today trans = concatMap (go (getDay $ head trans))
                $ groupBy ((==) `on` getAccountType) trans
   where go day (t:ts) = if today > addGregorianMonthsClip 1 day
            then Transaction day (negate . sum . map getCent
                 $ takeWhile ((>) (addGregorianMonthsClip 1 day) . getDay)
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

getAccounts' :: IOSArrow XmlTree Account
getAccounts' = deepName "gnc:account"
            >>> (deepName "act:id" /> getText
                &&& deepName "act:type" /> getText)
            >>> second (arr toAccountType)

getTransactions :: [Account] -> IOSArrow XmlTree Transaction
getTransactions accounts = deepName "gnc:transaction"
            >>> (deepName "trn:date-posted" /> hasName "ts:date"
                &&& (deepName "trn:split"
                    >>> (deepName "split:value"
                        &&& deepName "split:account")))
            >>> deep getText
                *** deep getText
                *** deep getText
            >>> arr (\(a, (b,c)) -> Transaction (toDate $ T.pack a)
                                                (toMoney b) c
                                                (toAccount c))
  where toMoney = read . takeWhile (/= '/')
        toDate = either error id . A.parseOnly (do
             [y, m, d] <- A.count 3 $ A.decimal <* A.anyChar
             return $ fromGregorian y (fromInteger m) (fromInteger d))
        toAccount key = fromMaybe (error "a") $ lookup key accounts

getAccounts :: IOSArrow XmlTree Account
getAccounts = proc input -> do
            a <- deepName "gnc:account"         -< input
            b <- deepName "act:id"   /> getText -< a
            c <- deepName "act:type" /> getText -< a
            d <- arr toAccountType -< c
            returnA-< (b,d)

deepName :: String -> IOSArrow XmlTree XmlTree
deepName = deep . hasName
