module StockProfits where

import Prelude

import Data.Array (concat, head, snoc, sortBy, tail, reverse, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

type Price = Int

type Day = Int

type StockDay = Tuple Price Day

data BuySell = BuySell (StockDay) (StockDay)

type BestCandidate = Tuple BuySell (Maybe BuySell)

newtype SortedArray a = SortedArray (Array a)

-- derive newtype instance showSortedArrayInt :: Show (SortedArray Int)
-- derive newtype instance showSortedArrayStockDay :: Show (SortedArray StockDay)
-- derive newtype instance showSortedArrayTradePair :: Show (SortedArray BuySell)

instance showTradePair :: Show BuySell where
  show (BuySell a b) = "BuySell " <> show a <> " | " <> show b

instance showSortedArray :: Show a => Show (SortedArray a) where
  show (SortedArray a) = "SortedArray " <> show a

isValidTradeDayOrder :: BuySell -> BuySell -> Boolean
isValidTradeDayOrder (BuySell (Tuple _ a) (Tuple _ b)) (BuySell (Tuple _ c) (Tuple _ d)) =
  (a < b && c < d && c > b) || (c < d && a < b && d < a)

bestBuySellPair :: BuySell -> SortedArray (BuySell) -> Int -> Maybe BestCandidate -> Maybe BestCandidate
bestBuySellPair buySell (SortedArray []) maxProfitSoFar bestTradeSoFar =
  if buySellProfit buySell > maxProfitSoFar then (Just $ Tuple buySell Nothing)
  else bestTradeSoFar
bestBuySellPair buySell (SortedArray tradePairs) maxProfitSoFar bestTradeSoFar =
  case head tradePairs of
    Just h ->
      if (isValidTradeDayOrder buySell h && (buySellProfit buySell + buySellProfit h) > maxProfitSoFar) then bestBuySellPair buySell (fromMaybe (SortedArray []) (map SortedArray $ tail tradePairs)) (buySellProfit buySell + buySellProfit h) (Just $ Tuple buySell (Just h))
      else bestBuySellPair buySell (fromMaybe (SortedArray []) (map SortedArray $ tail tradePairs)) maxProfitSoFar bestTradeSoFar
    Nothing ->
      if buySellProfit buySell > maxProfitSoFar then (Just $ Tuple buySell Nothing)
      else bestTradeSoFar

buySellProfit :: BuySell -> Int
buySellProfit (BuySell a b) = fst b - fst a

getFirst :: BuySell -> StockDay
getFirst (BuySell a _) = a

getSecond :: BuySell -> StockDay
getSecond (BuySell _ b) = b

workThroughStockDays :: SortedArray BuySell -> Maybe BestCandidate
workThroughStockDays tradePairs = go tradePairs 0 (Nothing)
  where
  go :: SortedArray BuySell -> Int -> Maybe BestCandidate -> Maybe BestCandidate
  go (SortedArray []) _ candidate = candidate
  go (SortedArray tps) maxProfitSoFar candidate =
    case head tps of
      Just tp ->
        let
          candidate1 = bestBuySellPair tp (SortedArray tps) maxProfitSoFar candidate
          bestCandidate = case candidate, candidate1 of
            Just c1, Just c2 -> Just $ getBestCandidate c1 c2
            Just c1, Nothing -> Just c1
            Nothing, Just c2 -> Just c2
            _, _ -> Nothing
          newMaxProfitSoFar = map candidateProfit bestCandidate # fromMaybe 0
        in
          go (fromMaybe (SortedArray []) $ map SortedArray $ tail tps) newMaxProfitSoFar bestCandidate
      Nothing -> candidate

getBestCandidate :: BestCandidate -> BestCandidate -> BestCandidate
getBestCandidate c1 c2 =
  if candidateProfit c2 > candidateProfit c1 then c2 else c1

candidateProfit :: BestCandidate -> Int
candidateProfit (Tuple t1 Nothing) = buySellProfit t1
candidateProfit (Tuple t1 (Just t2)) = buySellProfit t1 + buySellProfit t2

arrayToStockDay :: Array Int -> SortedArray StockDay
arrayToStockDay xs = sortStockDay $ go xs 1 []
  where
  go :: Array Int -> Int -> Array StockDay -> Array StockDay
  go [] _ acc = acc
  go ys day acc =
    case head ys of
      Just stockPrice -> go (fromMaybe [] $ tail ys) (day + 1) (snoc acc (Tuple stockPrice day))
      Nothing -> acc

  sortStockDay :: Array StockDay -> SortedArray StockDay
  sortStockDay ls = SortedArray $ sortBy (\sdp1 sdp2 -> if fst sdp1 < fst sdp2 then LT else GT) ls

makeBuySell :: StockDay -> StockDay -> Maybe BuySell
makeBuySell t1@(Tuple p1 a) t2@(Tuple p2 b) =
  if a < b && p1 < p2 then Just (BuySell t1 t2) else Nothing

makeBuySellList :: StockDay -> SortedArray StockDay -> Array BuySell
makeBuySellList sdp (SortedArray xs) = go sdp xs []
  where
  go _ [] acc = acc
  go _sdp ls acc = case head ls of
    Just h -> case makeBuySell _sdp h of
      Nothing -> go _sdp (fromMaybe [] $ tail ls) acc
      Just tp -> go _sdp (fromMaybe [] $ tail ls) (snoc acc tp)
    Nothing -> acc

makeBuySellCombinationsList :: SortedArray StockDay -> SortedArray BuySell
makeBuySellCombinationsList xs = SortedArray $ reverse $ sortBy sortBuySell $ go xs ([])
  where
  go :: SortedArray StockDay -> Array BuySell -> Array BuySell
  go (SortedArray []) tps = tps
  go (SortedArray sdps) (tps) =
    case head sdps of
      Nothing -> (tps)
      Just h -> go (SortedArray $ fromMaybe [] $ tail sdps) (concat [ tps, makeBuySellList h (SortedArray $ fromMaybe [] $ tail sdps) ])

arrayToBuySellList :: Array Int -> SortedArray BuySell
arrayToBuySellList = arrayToStockDay >>> makeBuySellCombinationsList

findBestCandidate :: Array Int -> Maybe BestCandidate
findBestCandidate = arrayToBuySellList >>> workThroughStockDays

profitFromBestCandidate :: BestCandidate -> Int
profitFromBestCandidate (Tuple tp1 Nothing) = buySellProfit tp1
profitFromBestCandidate (Tuple tp1 (Just tp2)) = buySellProfit tp1 + buySellProfit tp2

sortBuySell :: BuySell -> BuySell -> Ordering
sortBuySell bs1 bs2 =
  if buySellProfit bs1 > buySellProfit bs2 then GT else LT

lengthSortedArray :: forall a. SortedArray a -> Int
lengthSortedArray (SortedArray xs) = length xs

totalProfits :: Array Int -> Int
totalProfits = findBestCandidate >>> map profitFromBestCandidate >>> fromMaybe 0