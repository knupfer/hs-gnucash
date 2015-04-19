{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Data.List
import System.Environment
import Control.Arrow
import Text.XML.HXT.Core

data Date = Date Int Int Int deriving (Eq, Show, Ord)
data Cent = Cent Int deriving (Eq, Show, Ord)
data AccountType = Asset | Receivable | Liability | Payable | Income | Expense | Equity | Bank deriving (Eq, Show)
type AccountId = String
type Account = (AccountId, AccountType)
data Transaction = Transaction Date Cent Account deriving (Eq, Show)

main :: IO ()
main = do
--  [src] <- getArgs
  gnucash <- readFile "test.gnucash"
  let doc = readString [withParseHTML yes, withWarnings no] gnucash
  accounts     <- runX $ doc >>> getAccounts
  transactions <- runX $ doc >>> getTransactions accounts

  print $ take 10 transactions
--  print $ sort transactions

getAccounts = deep $ hasName "gnc:account"
            >>> (deep (hasName "act:id" /> getText)
                &&& deep (hasName "act:type" /> getText))
         >>> arr toAccount
    where toAccount (a,b) = (a, case b of
                           "ASSET" -> Asset
                           "RECEIVABLE" -> Receivable
                           "LIABILITY" -> Liability
                           "PAYABLE" -> Payable
                           "INCOME" -> Income
                           "EXPENSE" -> Expense
                           "EQUITY" -> Equity
                           "BANK" -> Bank
                           _      -> Bank)

getTransactions accounts  = deep $ hasName "gnc:transaction"
            >>> (deep (hasName "trn:date-posted" /> hasName "ts:date")
                &&& (deep (hasName "split:value") &&& deep (hasName "split:account")))
            >>> deep getText *** deep getText *** deep getText
            >>> arr (\(a, (b,c)) -> Transaction (toDate $ T.pack a) (toMoney b) (toAccount c))
  where toMoney = Cent . read . takeWhile (/= '/')
        toDate = either error id . A.parseOnly (do
             [y,m,d] <- A.count 3 $ A.decimal <* A.anyChar
             return (Date y m d))
        toAccount key = maybe (error "a") ((,) key) $ lookup key accounts
