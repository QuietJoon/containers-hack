module TestCases where

import Data.IntMap.Bound
import Data.IntMap.Bound.Verifier

main = do
  tView roughBound boundedV 1 5 [0]
  tView roughBound boundedV 2 4 [0,3]
  tView roughBound boundedV 0 0 [1]
  tView roughBound boundedV 4 4 [5,6]
  tView roughBound boundedV 2 3 [0,2]
  tView roughBound boundedV 2 4 [3,2,4,0]
  tView roughBound boundedV 0 24 [0,16,17,18]