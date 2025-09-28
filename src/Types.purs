module Types
  ( StockDay(..)
  , Price
  , Day
  , BuySell(..)
  , BestCandidate
  , SortedArray
  , buySellProfit
  , toSorted
  , fromSorted
  , mkReverseSortedArray
  , class ValidTrade
  , validTrade
  , (??)
  , head
  , tail
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

type Price = Int

type Day = Int

data StockDay = StockDay Price Day

data BuySell = BuySell (StockDay) (StockDay)

newtype SortedArray a = SortedArray (Array a)

type BestCandidate = Tuple BuySell (Maybe BuySell)

instance showStockDay :: Show StockDay where
  show (StockDay p d) = "StockDay " <> show p <> " " <> show d

instance showTradePair :: Show BuySell where
  show (BuySell a b) = "BuySell (" <> show a <> ") (" <> show b <> ")"

instance eqStockDay :: Eq StockDay where
  eq (StockDay p1 d1) (StockDay p2 d2) = p1 == p2 && d1 == d2

instance eqBuySell :: Eq BuySell where
  eq (BuySell a1 b1) (BuySell a2 b2) = a1 == a2 && b1 == b2

instance ordStockDay :: Ord StockDay where
  compare = sortStockDay

instance ordBuySell :: Ord BuySell where
  compare = sortBuySell

instance showSortedArray :: Show a => Show (SortedArray a) where
  show (SortedArray arr) = "SortedArray " <> show arr

sortStockDay :: StockDay -> StockDay -> Ordering
sortStockDay (StockDay p1 _) (StockDay p2 _) =
  if p1 < p2 then LT else GT

sortBuySell :: BuySell -> BuySell -> Ordering
sortBuySell bs1 bs2 =
  if buySellProfit bs1 > buySellProfit bs2 then GT else LT

buySellProfit :: BuySell -> Int
buySellProfit (BuySell (StockDay p1 _) (StockDay p2 _)) = p2 - p1

toSorted :: forall a. Ord a => Array a -> SortedArray a
toSorted arr = SortedArray (Array.sort arr)

fromSorted :: forall a. SortedArray a -> Array a
fromSorted (SortedArray arr) = arr

mkReverseSortedArray :: forall a. Ord a => Array a -> SortedArray a
mkReverseSortedArray xs = SortedArray (Array.reverse $ Array.sort xs)

class ValidTrade a where
  validTrade :: a -> a -> Boolean

infix 1 validTrade as ??

instance ValidTrade BuySell where
  validTrade (BuySell s1 s2) (BuySell s3 s4) =
    validTrade s1 s2 && validTrade s2 s3 && validTrade s3 s4

instance ValidTrade StockDay where
  validTrade (StockDay _ d1) (StockDay _ d2) = d1 < d2

head :: forall a. Ord a => SortedArray a -> Maybe a
head (SortedArray xs) = Array.head xs

tail :: forall a. Ord a => SortedArray a -> Maybe (SortedArray a)
tail (SortedArray xs) = case Array.tail xs of
  Just ys -> Just (SortedArray ys)
  Nothing -> Nothing

instance semigroupSortedArray :: Ord a => Semigroup (SortedArray a) where
  append (SortedArray x) (SortedArray y) = toSorted (x <> y)
