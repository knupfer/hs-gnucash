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
                 deriving (Eq, Show)
type AccountId = String
type Account = (AccountId, AccountType)
data Transaction = Transaction { getDay     :: Day
                               , getCent    :: Cent
                               , getAccount :: Account
                               } deriving (Eq)

instance Show Transaction where
  show (Transaction d c a) = unwords [show d,show c,show a]

main :: IO ()
main = do
  src:xs  <- getArgs
  gnucash <- readFile src
  let doc = readString [withParseHTML yes, withWarnings no] gnucash
  accounts     <- runX $ doc >>> getAccounts
  transactions <- runX $ doc >>> getTransactions accounts
  mapM_ print . sortOn getDay $ filterAccounts xs transactions

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
