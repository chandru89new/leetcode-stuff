module StockProfits where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Types (BestCandidate, BuySell(..), SortedArray, StockDay(..), buySellProfit, head, mkArraySorted, mkReverseSortedArray, tail, (??))

main :: Effect Unit
main = do
  log "🍝"

bestBuySellPair :: BuySell -> SortedArray BuySell -> Int -> Maybe BestCandidate -> Maybe BestCandidate
bestBuySellPair buySell tradePairs maxProfitSoFar bestTradeSoFar =
  case head tradePairs of
    Just h ->
      if ((buySell ?? h) && (buySellProfit buySell + buySellProfit h) > maxProfitSoFar) then bestBuySellPair buySell (fromMaybe (mkArraySorted []) (tail tradePairs)) (buySellProfit buySell + buySellProfit h) (Just $ Tuple buySell (Just h))
      else bestBuySellPair buySell (fromMaybe (mkArraySorted []) (tail tradePairs)) maxProfitSoFar bestTradeSoFar
    Nothing ->
      if buySellProfit buySell > maxProfitSoFar then (Just $ Tuple buySell Nothing)
      else bestTradeSoFar

workThroughStockDays :: SortedArray BuySell -> Maybe BestCandidate
workThroughStockDays tradePairs = go tradePairs 0 (Nothing)
  where
  go :: SortedArray BuySell -> Int -> Maybe BestCandidate -> Maybe BestCandidate
  go tps maxProfitSoFar candidate =
    case head tps of
      Just tp ->
        let
          candidate1 = bestBuySellPair tp tps maxProfitSoFar candidate
          bestCandidate = case candidate, candidate1 of
            Just c1, Just c2 -> Just $ getBestCandidate c1 c2
            Just c1, Nothing -> Just c1
            Nothing, Just c2 -> Just c2
            _, _ -> Nothing
          newMaxProfitSoFar = map candidateProfit bestCandidate # fromMaybe 0
        in
          go (fromMaybe (mkArraySorted []) $ tail tps) newMaxProfitSoFar bestCandidate
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
    case Array.head ys of
      Just stockPrice -> go (fromMaybe [] $ Array.tail ys) (day + 1) (Array.snoc acc (StockDay stockPrice day))
      Nothing -> acc

  sortStockDay :: Array StockDay -> SortedArray StockDay
  sortStockDay = mkArraySorted

makeBuySell :: StockDay -> StockDay -> Maybe BuySell
makeBuySell t1@(StockDay p1 _) t2@(StockDay p2 _) =
  if (t1 ?? t2) && p1 < p2 then Just (BuySell t1 t2) else Nothing

makeBuySellList :: StockDay -> SortedArray StockDay -> Array BuySell
makeBuySellList sdp xs = go sdp xs []
  where
  go :: StockDay -> SortedArray StockDay -> Array BuySell -> Array BuySell
  go _sdp ls acc = case head ls of
    Just h -> case makeBuySell _sdp h of
      Nothing -> go _sdp (fromMaybe (mkArraySorted []) $ tail ls) acc
      Just tp -> go _sdp (fromMaybe (mkArraySorted []) $ tail ls) (Array.snoc acc tp)
    Nothing -> acc

makeBuySellCombinationsList :: SortedArray StockDay -> SortedArray BuySell
makeBuySellCombinationsList xs = mkReverseSortedArray $ go xs ([])
  where
  go :: SortedArray StockDay -> Array BuySell -> Array BuySell
  go sdps tps =
    case head sdps of
      Nothing -> tps
      Just h -> go (fromMaybe (mkArraySorted []) $ tail sdps) (Array.concat [ tps, makeBuySellList h (fromMaybe (mkArraySorted []) $ tail sdps) ])

arrayToBuySellList :: Array Int -> SortedArray BuySell
arrayToBuySellList = arrayToStockDay >>> makeBuySellCombinationsList

findBestCandidate :: Array Int -> Maybe BestCandidate
findBestCandidate = arrayToBuySellList >>> workThroughStockDays

profitFromBestCandidate :: BestCandidate -> Int
profitFromBestCandidate (Tuple tp1 Nothing) = buySellProfit tp1
profitFromBestCandidate (Tuple tp1 (Just tp2)) = buySellProfit tp1 + buySellProfit tp2

totalProfits :: Array Int -> Int
totalProfits = findBestCandidate >>> map profitFromBestCandidate >>> fromMaybe 0

assert :: Boolean -> Effect Unit
assert bool = log $ if bool then "Test passed" else "Failed"

runTests :: Effect Unit
runTests = do
  assert $ totalProfits [ 3, 1, 0, 0, 5, 4, 1, 3 ] == 7
  assert $ totalProfits [ 3, 1, 0, 0, 5, 4, 1 ] == 5
  assert $ totalProfits [ 3, 1, 0, 0 ] == 0
  assert $ totalProfits [ 3, 1, 0, 0, 1 ] == 1
  assert $ totalProfits [ 3, 1, 0, 0, 1, 2, 3 ] == 3