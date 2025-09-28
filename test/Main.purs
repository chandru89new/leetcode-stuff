module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import StockProfits (totalProfits)

assert :: Boolean -> Effect Unit
assert bool = log $ if bool then "Test passed" else "Failed"

main :: Effect Unit
main = do
  assert $ totalProfits [ 3, 1, 0, 0, 5, 4, 1, 3 ] == 7
  assert $ totalProfits [ 3, 1, 0, 0, 5, 4, 1 ] == 5
  assert $ totalProfits [ 3, 1, 0, 0 ] == 0
  assert $ totalProfits [ 3, 1, 0, 0, 1 ] == 1
  assert $ totalProfits [ 3, 1, 0, 0, 1, 2, 3 ] == 3