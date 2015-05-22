{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Time
import           Filesystem.Path.CurrentOS (decodeString)
import           Safe
import           System.Environment
import           Text.XML                  hiding (readFile)
import qualified Text.XML                  as X (readFile)
import           Text.XML.Cursor

type AccountName = T.Text
type ParentName = T.Text
type AccountM = M.Map AccountId Account
type AccountId = T.Text

data Cent = Cent Integer deriving (Eq)

instance Show Cent where
  show (Cent n) = show (n `div` 100) ++ "." ++ show (n `mod` 100)
                 ++ if n `mod` 10 == 0 then "0" else ""

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

data Account = Account { getAccountName :: AccountName
                       , getType        :: AccountType
                       , getParent      :: Maybe Account
                       } deriving (Show, Eq, Ord)

data Split = Split { getCent    :: Cent
                   , getAccount :: Account
                   } deriving (Show, Eq)

data Transaction = Transaction { getDay             :: Day
                               , getTransactionName :: T.Text
                               , getSplits          :: [Split]
                               } deriving (Eq, Show)

main :: IO ()
main = do
  file:args <- getArgs
  today     <- utctDay      <$> getCurrentTime
  cursor    <- fromDocument <$> X.readFile def (decodeString file)
  putStr     $ output (parseArgs today args) cursor

parseArgs :: Day -> [String] -> [Transaction] -> String
parseArgs _ [] = const "err"
parseArgs day (x:xs) = fromMaybe (parseArgs day []) . join $ lookup x
  [ ("bin"      , do
                  times <- headMay xs        >>= readMay
                  size  <- headMay (tail xs) >>= readMay
                  let accs = drop 2 xs
                  return $ toCsv
                         . foldl1 (.) (replicate times $ bin size)
                         . map (\(Transaction d _ [Split (Cent c) a])
                                -> Transaction d "NA" [Split (Cent $ negate c*size) a])
                         . filterAccounts (map T.pack accs)
                         . (\t -> getProfit t ++ t)
                         . noFuture day
                         . concatMap toSingleBook)
  , ("integrate", return $ toCsv
                         . map (\(Transaction d _ [Split (Cent c) a])
                                  -> Transaction d "NA" [Split (Cent $ negate c) a])
                         . integrate
                         . filterAccounts (map T.pack xs)
                         . (\t -> getProfit t ++ t)
                         . noFuture day
                         . concatMap toSingleBook)
  , ("active"   , return $ toCsv
                         . noFuture day
                         . integrate
                         . (\t -> getProfit t ++ t)
                         . activeAccounts
                         . filterAccounts ["INCOME"]
                         . concatMap toSingleBook)
  , ("density"  , return $ toCsv
                         . density day
                         . filterAccounts ["INCOME"]
                         . concatMap toSingleBook)
  , ("age"      , return $ toCsv
                         . noFuture day
                         . meanAge day
                         . filterAccounts ["INCOME"]
                         . concatMap toSingleBook)
  ]

activeAccounts :: [Transaction] -> [Transaction]
activeAccounts ts = concatMap go $ groupBy ((==) `on` getAccount . head . getSplits)
                    $ sortOn (getAccount . head . getSplits &&& getDay) ts
  where go ts' = Transaction (getDay $ head ts')
                             "NA" [Split (Cent 100) (Account "NA" Income Nothing)]
                 : [Transaction (getDay $ last ts')
                                "NA" [Split (Cent $ negate 100) (Account "NA" Expense Nothing)]]

meanAge :: Day -> [Transaction] -> [Transaction]
meanAge d ts = zipWith foo [1..] $ integrate $ sortOn getDay $ concatMap go $ groupBy ((==) `on` getAccount . head . getSplits)
                                 $ sortOn (getAccount . head . getSplits &&& getDay) ts
  where go ts' = [Transaction
                    (getDay $ last ts')
                    "NA" [Split (Cent $ (100 * diffDays (min d . getDay $ last ts') (getDay $ head ts')) `div` 31)
                  (Account "NA" Expense Nothing)]]
        foo i (Transaction ds n ((Split (Cent c) a):_)) = Transaction ds n [Split (Cent (c`div`i)) a]

density :: Day -> [Transaction] -> [Transaction]
density d ts = concatMap go $ groupBy ((==) `on` getAccount . head . getSplits)
                            $ sortOn (getAccount . head . getSplits &&& getDay) ts
  where go ts' = [Transaction (addDays (diffDays (min d . getDay $ last ts') (getDay $ head ts')) $ fromGregorian 0 0 0)
                              "NA" [Split (Cent 100) (Account "NA" (if d < getDay (last ts')
                                                                       then Income
                                                                       else Expense) Nothing)]]

output :: ([Transaction] -> String) -> Cursor -> String
output fun cursor = fun  $ getTransactions cursor (getAccounts cursor)

integrate :: [Transaction] -> [Transaction]
integrate xs = concatMap go $ groupBy ((==) `on` theirType) sortedTrans
  where sortedTrans   = sortOn (theirType &&& getDay) $ concatMap toSingleBook xs
        go ys  = scanl1 mup ys :: [Transaction]
        mup (Transaction _ _ (Split (Cent c) _ :_)) (Transaction d n (Split (Cent c') a:_)) =
             Transaction d n [Split (Cent $ c + c') a]

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
toLedger (Transaction d n ss) = T.unlines $
         [T.pack (show d) <> " " <> n] <> map printSplit ss <> [""]
  where getAH x = getAccountName x : maybe [] getAH (getParent x)
        printSplit (Split x y) = let t = "  " <> T.intercalate ":" (getAH y) <> "  "
                                 in T.justifyLeft (80 - length (show x)) ' ' t <> T.pack (show x)

bin :: Integer -> [Transaction] -> [Transaction]
bin s trans = concat $ mapMaybe (\x -> do
  l <- boundaryDay lastMay x
  h <- boundaryDay headMay x
  return . filter ((>) l . addDays (s `div` 4) . getDay) $ bin' s (h, l) x)
    $ groupBy ((==) `on` theirType) sortedTrans
  where sortedTrans   = sortOn (theirType &&& getDay) $ concatMap toSingleBook trans
        boundaryDay f = fmap getDay .  f . sortOn getDay

theirType :: Transaction -> Maybe AccountType
theirType = fmap (getType . getAccount) . headMay . getSplits

bin' :: Integer -> (Day, Day) -> [Transaction] -> [Transaction]
bin' s (fDay, lDay) ts = Transaction fDay "NA"
  (catMaybes [ do a <- headMay ts
                  b <- theirType a
                  return . split $ Account "NA" b Nothing
  ]) : if fDay < lDay
          then bin' s (addDays 1 fDay, lDay) ts
          else []
  where split = Split ( Cent . flip div (minimum [s, 1+diffDays lDay fDay]) . sum
                      . mapMaybe (fmap ((\(Cent x) -> x) . getCent) . headMay . getSplits)
                      . takeWhile ((>) (addDays s fDay) . getDay)
                      $ dropWhile ((>) fDay . getDay) ts
                      )

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
parseTransaction accounts c = catMaybes [do a <- headMay . reads $ T.unpack date
                                            return $ Transaction (fst a) desc h]
  where splits   =           c $/ laxElement "splits"      &/ laxElement "split" >=> split
        date     = mconcat $ c $/ laxElement "date-posted" &/ getElement "date"
        desc     = mconcat $ c $/ getElement "description"
        val i    =           i $/ getElement "value"
        acc i    =           i $/ getElement "account"
        split c' = [(headMay . val &&& headMay . acc) c']
        f x      = fmap (Cent . fst) . headMay . reads $ T.unpack x
        g x      = M.lookup x accounts
        h        = mapMaybe (\(x,y) -> do
                              x' <- x >>= f
                              y' <- y >>= g
                              return $ Split x' y'
                            ) splits
        getElement n = laxElement n &/ content
