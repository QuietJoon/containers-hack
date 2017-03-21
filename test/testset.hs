module TestCases where

import Data.IntMap.Bound
import Data.IntMap.Bound.Verifier

main = do
  tView boundedOf boundedV 1 5  [0]
  tView boundedOf boundedV 2 4  [0,3]
  tView boundedOf boundedV 0 0  [1]
  tView boundedOf boundedV 4 4  [5,6]
  tView boundedOf boundedV 2 3  [0,2]
  tView boundedOf boundedV 2 4  [3,2,4,0]
  tView boundedOf boundedV 0 24 [0,16,17,18]
  tView boundedOf boundedV 0 0  [1]
  tView boundedOf boundedV 0 0  [1,2]
  tView boundedOf boundedV 0 0  [0,-1]
  tView boundedOf boundedV 0 0  [1,-1]
  tView boundedOf boundedV (-1) 0 [1,2]
  tView boundedOf boundedV 2 2  [-1,0]
