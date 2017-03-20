module Data.IntMap.Bound where


import Data.IntMap.Bound.Base

import Prelude as P

import Data.IntMap.Internal as I
import Data.Maybe

import Debug.Shortcut

-- Handle Positive/Negative only case
roughBound lb ub = \t ->
  case t of
    (Bin p m l r) -> goU Nil t Nil
    _ -> goG t
  where
    goU ld tree@(Bin p m l r) rd
      -- How to clear unnecessary ld/rd
      | nomatch lb p m && lb > p = -- traceX "a_" $
          goU l r rd
      | nomatch ub p m && ub < p = -- traceX "b_" $
          goU ld l r
      -- | nomatch lb p m = traceX "A'" $ goU l r rd
      -- | nomatch ub p m = traceX "B'" $ goU ld l r

      -- when match, there is no problem, but when nomatch
      -- | zero ub m = traceX "C" $
      | match ub p m && zero ub m = -- traceX "c_" $
          goU ld l r
      -- | not (zero lb m) = traceX "D" $ goU l r rd
      | match lb p m && not (zero lb m) = -- traceX "d_" $
          goU l r rd
      | otherwise = -- traceX "e_" $
          (ld, tree, rd)
    goU ld tree rd = (ld,tree,rd)
    goG tip@(Tip k _)
      | k < lb = (tip, Nil, Nil)
      | k > ub = (Nil, Nil, tip)
      | otherwise = (Nil, tip, Nil)
    goG Nil = (Nil, Nil, Nil)

-- bound guarantees
-- (ld,bd,rd): (findMax ld) < lb , lb <= (findMin bd) , (findMax bd) <= ub , ub < (findMin rd)
-- roughBound does not guarantee any conditions.
bounded :: Show a => Key -> Key -> IntMap a -> [(Key,a)]
-- bound :: Key -> Key -> IntMap a -> [(Key,a)]
bounded lb ub t = traceS (roughBound lb ub t) $
  case bd of
    (Bin p m l r) -> traceX "J" $ case () of
      _ | m > 0 || p < 0 -> traceX "'" $ goL ld l (goR r rd [])
        | 0 <= lb -> traceX "." $ bounded lb ub l
        | ub < 0 -> traceX ":" $ bounded lb ub r
        -- ((findMin' t) < 0 && 0 <= (findMax' t)) && (lb < 0 && 0 <= ub)
        | otherwise -> traceX "-" $ goL nld nbd (goR pbd prd [])
            where
              (_,pbd,prd) = roughBound 0 ub l
              (nld,nbd,_) = roughBound lb (-1) r
    (Tip k _) -> case () of
      _ | k >= ub -> traceX "K" $ goL Nil ld (goR bd rd [])
        | k <= lb -> traceX "M" $ goL ld bd (goR rd Nil [])
        | otherwise -> traceX "N" $ goL ld bd (goR rd Nil [])
    Nil -> goL Nil ld (goR rd Nil [])
  where
    (ld,bd,rd) = roughBound lb ub t
    -- goR: assume that `lb <= findMin base`.
    goR (Bin p m l r) rd aList
      -- assume that there is no Nil in Bin
      -- | nomatch ub p m && ub < p = goR l r aList
      -- | nomatch ub p m && ub < p = goR l r []
      | nomatch ub p m && ub < p = traceX "V" $ goR l Nil []
      -- assume that not `I.null r`
      | match ub p m && zero ub m = traceX "W" $ goR l r []
      | otherwise = traceX "X" $ go l (goR r rd [])

    goR (Tip k v) rd aList
      | ub < k = traceX "Y" $ [(k,v)]
      | otherwise = traceX "Z" $ (k,v) : goR rd Nil aList

    goR Nil _ _ = []

    goL ld (Bin p m l r) aList
      | nomatch lb p m && lb > p = traceX "A" $ goL Nil r aList
      | match lb p m && not (zero lb m) = traceX "B" $ goL l r aList
      | otherwise = traceX "C" $ goL ld l (go r aList)

    goL ld (Tip k v) aList
      | k < lb = traceX "D" $ (k,v) : aList
      | otherwise = traceX "E" $ goL Nil ld ((k,v):aList)

    goL _ Nil aList = aList

    go (Bin _ _ l r) aList = go l (go r aList)
    go (Tip k v) aList = (k,v):aList
    go Nil aList = aList


