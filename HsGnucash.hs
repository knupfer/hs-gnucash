{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Data.List
import System.Environment
import Control.Arrow
import Text.XML.HXT.Core
import Data.Time

data Date = Date Integer Int Int deriving (Eq, Ord)
type Cent = Int
data AccountType = Asset
                 | Bank
                 | Equity
                 | Expense
                 | Income
                 | Liability
                 | Payable
                 | Receivable
                 deriving (Eq, Show, Ord)
type AccountId = String
type Account = (AccountId, AccountType)
data Transaction = Transaction { getDay     :: Day
                               , getCent    :: Cent
                               , getAccount :: Account
                               } deriving (Eq)

instance Show Transaction where
  show (Transaction d c a) = unwords [ show . (\(yyyy,mm,dd) ->
                                         fromIntegral yyyy
                                         + (fromIntegral mm
                                           + fromIntegral dd / 30.5) / 12)
                                         $ toGregorian d
                                     , show c, fst a, show $ snd a]

main :: IO ()
main = do
  src:xs  <- getArgs
  day <- utctDay <$> getCurrentTime
  gnucash <- readFile src
  let doc = readString [withParseHTML yes, withWarnings no] gnucash
  accounts     <- runX $ doc >>> getAccounts
  transactions <- runX $ doc >>> getTransactions accounts
  putStrLn "Date Money Id Account"
  mapM_ print . bin day . sortTransaction $ filterAccounts xs transactions
  where sortTransaction = sortOn (snd . getAccount) . sortOn getDay

bin :: Day -> [Transaction] -> [Transaction]
bin today trans = concatMap (go (getDay $ head trans))
                $ groupBy (\x y -> snd (getAccount x) == snd (getAccount y)) trans
   where go day (t:ts) = if today < addGregorianMonthsClip 1 day
            then []
            else Transaction day (sum . map getCent
                                 $ takeWhile ((>) (addGregorianMonthsClip 1 day) . getDay)
                                 $ dropWhile ((>) day . getDay) ts) ("NA", snd $ getAccount t) : go (addDays 1 day) (t:ts)

filterAccounts :: [String] -> [Transaction] -> [Transaction]
filterAccounts xs = filter $ flip elem (map toAccountType xs) . snd . getAccount

toAccountType :: String -> AccountType
toAccountType b = case b of
  "ASSET"      -> Asset
  "RECEIVABLE" -> Receivable
  "LIABILITY"  -> Liability
  "PAYABLE"    -> Payable
  "INCOME"     -> Income
  "EXPENSE"    -> Expense
  "EQUITY"     -> Equity
  _            -> Bank

getAccounts :: IOSArrow XmlTree Account
getAccounts = deep $ hasName "gnc:account"
            >>> (deep (hasName "act:id" /> getText)
                &&& deep (hasName "act:type" /> getText))
            >>> second (arr toAccountType)

getTransactions :: [Account] -> IOSArrow XmlTree Transaction
getTransactions accounts = deep $ hasName "gnc:transaction"
            >>> (deep (hasName "trn:date-posted" /> hasName "ts:date")
                &&& (deep (hasName "trn:split")
                    >>> (deep (hasName "split:value")
                        &&& deep (hasName "split:account"))))
            >>> deep getText *** deep getText *** deep getText
            >>> arr (\(a, (b,c)) -> Transaction (toDate $ T.pack a)
                                               (toMoney b)
                                               (toAccount c))
  where toMoney = read . takeWhile (/= '/')
        toDate = either error id . A.parseOnly (do
             [y, m, d] <- A.count 3 $ A.decimal <* A.anyChar
             return $ fromGregorian y (fromInteger m) (fromInteger d))
        toAccount key = maybe (error "a") ((,) key) $ lookup key accounts
