module Data.IntMap.Bound where


import Data.IntMap.Bound.Base

import Prelude as P

import Data.IntMap.Internal as I
import Data.Maybe

import Debug.Shortcut

-- Handle Positive/Negative only case
roughBound lb ub = \t ->
  case t of
    (Bin p m l r) -> case () of
      _ | m > 0 || p < 0 -> def $ goU Nil t Nil
        | 0 <= lb -> def $ if I.null lld then (r,lbd,lrd) else (lld,lbd,lrd)
        | ub <  0 -> def $ if I.null rrd then (rld,rbd,l) else (rld,rbd,rrd)
        | otherwise ->
            if I.null nbd
              then (nld,pbd,Nil,prd)
              else (nld,nbd,pbd,prd)
            where
              (lld,lbd,lrd) = goU Nil l Nil
              (rld,rbd,rrd) = goU Nil r Nil
              (_,pbd,Nil,prd) = roughBound 0 ub l
              (nld,nbd,Nil,_) = roughBound lb (-1) r
    _ -> goG t

  where
    goU ld tree@(Bin p m l r) rd
      -- How to clear unnecessary ld/rd
      | nomatch lb p m && lb > p = goU l r rd
      | nomatch ub p m && ub < p = goU ld l r
      | match ub p m && zero ub m = goU ld l r
      | match lb p m && not (zero lb m) = goU l r rd
      | otherwise = (ld, tree, rd)
    goU ld tree rd = (ld,tree,rd)
    goG tip@(Tip k _)
      | k < lb = (tip, Nil, Nil, Nil)
      | k > ub = (Nil, Nil, Nil, tip)
      | otherwise = (Nil, tip, Nil, Nil)
    goG Nil = (Nil, Nil, Nil, Nil)
    def ~(l,b,r) = (l,b,Nil,r)

-- bounded guarantees
-- (ld,bd,rd): (findMax ld) < lb , lb <= (findMin bd) , (findMax bd) <= ub , ub < (findMin rd)
-- roughBound does not guarantee any conditions.
bounded :: Show a => Key -> Key -> IntMap a -> [(Key,a)]
bounded lb ub t = traceS (roughBound lb ub t) $
  if I.null nbd
    then
      case bd of
        (Bin _ _ l r) -> goL ld l (goR r rd [])
        (Tip k _) -> case () of
          _ | k >= ub -> goL Nil ld (goR bd rd [])
            | k <= lb -> goL ld bd (goR rd Nil [])
            | otherwise -> goL ld bd (goR rd Nil [])
        Nil -> goL Nil ld (goR rd Nil [])
    else
      goL ld bd (goR nbd rd [])
  where
    (ld,bd,nbd,rd) = roughBound lb ub t
    -- goR: assume that `lb <= findMin base`.
    goR (Bin p m l r) rd aList
      -- assume that there is no Nil in Bin
      | nomatch ub p m && ub < p = goR l Nil []
      -- assume that not `I.null r`
      | match ub p m && zero ub m = goR l r []
      | otherwise = go l (goR r rd [])

    goR (Tip k v) rd aList
      | ub < k = [(k,v)]
      | otherwise = (k,v) : goR rd Nil aList

    goR Nil _ _ = []

    goL ld (Bin p m l r) aList
      | nomatch lb p m && lb > p = goL Nil r aList
      | match lb p m && not (zero lb m) = goL l r aList
      | otherwise = goL ld l (go r aList)

    goL ld (Tip k v) aList
      | k < lb = (k,v) : aList
      | otherwise = goL Nil ld ((k,v):aList)

    goL _ Nil aList = aList

    go (Bin _ _ l r) aList = go l (go r aList)
    go (Tip k v) aList = (k,v):aList
    go Nil aList = aList
