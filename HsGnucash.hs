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
data Account = Asset | Receivable | Liability | Payable | Income | Expense | Equity | Bank
data Transaction = Transaction Date Cent String deriving (Eq, Show, Ord)

main :: IO ()
main = do
--  [src] <- getArgs
  gnucash <- readFile "transaction.gnucash"
  let doc = readString [withParseHTML yes, withWarnings no] gnucash
  transactions <- runX $ doc >>> getTransactions
  print $ sort transactions

getTransactions = deep $ hasName "gnc:transaction"
            >>> (deep (hasName "trn:date-posted" /> hasName "ts:date")
                &&& (deep (hasName "split:value") &&& deep (hasName "split:account")))
            >>> deep getText *** deep getText *** deep getText
            >>> arr (\(a, (b,c)) -> Transaction (date' $ T.pack a) (money b) c)
  where money = Cent . read . takeWhile (/= '/')
        date' = either error id . A.parseOnly (do
              y <- A.decimal <* A.char '-'
              m <- A.decimal <* A.char '-'
              d <- A.decimal
              return (Date y m d))
