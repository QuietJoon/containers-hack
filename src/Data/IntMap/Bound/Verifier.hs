module Data.IntMap.Bound.Verifier where

-- import Base
import Data.IntMap.Bound
import Data.IntMap.Bound.Base
import Data.IntMap.Hack.Tool
import Data.IntMap.Hack.Deprecated
import Data.IntMap.Internal as I


import Data.Bits as B
import Test.QuickCheck

import Debug.Shortcut


limitedV :: Int -> Int -> IntMap Int -> IntMap Int
limitedV lb ub =
  filterWithKey (\k _ -> lb <= k && k <= ub)

boundedV :: Int -> Int -> IntMap Int -> IntMap Int
boundedV lb ub t = boundedlr
  where
    limited = limitedV lb ub t
    lt = lookupLT lb t
    ut = lookupGT ub t
    boundedl  = traceS lt $ maybe limited (\(k,v) -> I.insert k v limited ) lt
    boundedlr = traceS ut $ maybe boundedl (\(k,v) -> I.insert k v boundedl) ut


bTester bf lbx ubx t
  | ( nf ld) && ( nf bd) && ( nf rd) = nf t
  | nf t = (nf ld) && (nf bd) && (nf rd)
  -- From here: `t` is not null

--  | ub < (findMin' t) = ( nf ld) && ( nf bd) && (nnf rd)
  | ( nf ld) && ( nf bd) && (nnf rd) = ub < (findMin' t) && (findMin' t) == (findMin' rd)

  | ( nf ld) && (nnf bd) && ( nf rd) = lb <= (findMin' bd) && (findMax' bd) <= ub && t == bd
--  | lb <= (findMin' t) && (findMax' t) <= ub = ( nf ld) && (nnf bd) && ( nf rd)

  | ( nf ld) && (nnf bd) && (nnf rd) = lb <= (findMin' bd) && (findMax' bd) <= ub && ub <  (findMin' rd) && (findMin' t) == (findMin' bd)

--  | (findMax' t) < lb = (nnf ld) && ( nf bd) && ( nf rd)
  | (nnf ld) && ( nf bd) && ( nf rd) = (findMax' ld) < lb && (findMax' t) == (findMax' ld)

  | (nnf ld) && ( nf bd) && (nnf rd) = (lookupGT' lb t) == (findMin' rd) && (lookupLT' ub t) == (findMax' ld)

  | (nnf ld) && (nnf bd) && ( nf rd) = (findMax' ld) <  lb && lb <= (findMin' bd) && (findMax' bd) <= ub && (findMax' t) == (findMax' bd)

  | (nnf ld) && (nnf bd) && (nnf rd) = (findMax' ld) <  lb && lb <= (findMin' bd) && (findMax' bd) <= ub && ub < (findMin' rd)

  where
    (ld,bd,rd) = bf lb ub t
    (lb,ub) = if (abs lbx) <= (abs ubx) then (abs lbx,abs ubx) else (abs ubx,abs lbx)


rbTester lbx ubx t
  | ( nf ld) && ( nf bd) && ( nf rd) = nf t
  | nf t = (nf ld) && (nf bd) && (nf rd)
  -- From here: `t` is not null

  -- rd /= t, but ub < (min rd) == (min t)
  | ( nf ld) && ( nf bd) && (nnf rd) = traceX "A" $
      ub < (findMin' t) && (findMin' t) == (findMin' rd)

  -- i) bd == t
  -- ii) when bd == (Tip k), lb <= k <= b
  -- iii) when bd == Bin, match
  | ( nf ld) && (nnf bd) && ( nf rd) = traceX "B" $
      -- (findMin' bd) <= lb && ub <= (findMax' bd) && t == bd
      case bd of
          -- (Bin _ _ _ _) -> (matchf lb bd && matchf ub bd) && t == bd
          (Bin _ _ _ _) -> inlb && inub && t == bd
          (Tip k _) -> lb <= k && k <= ub && t == bd
          _ -> error "[ERROR] rbTester: XXXX"

  | ( nf ld) && (nnf bd) && (nnf rd) = traceX "C" $
      -- inlb && inub && ub <  (findMin' rd) && (findMin' t) == (findMin' bd)
      -- do not need to guarantee about lower bound because `null ld`
      inlb && ub <  (findMin' rd) && (findMin' t) == (findMin' bd) && (findMax' bd) < (findMin' rd)

  | (nnf ld) && ( nf bd) && ( nf rd) = traceX "D" $
      (findMax' ld) < lb && (findMax' t) == (findMax' ld)

  | (nnf ld) && ( nf bd) && (nnf rd) = traceX "E" $
      (lookupGT' lb t) == (findMin' rd) && (lookupLT' ub t) == (findMax' ld)

  | (nnf ld) && (nnf bd) && ( nf rd) = traceX "G" $
      -- (findMax' ld) <  lb && inlb && inub && (findMax' t) == (findMax' bd)
      -- do not need to guarantee aboud upper bound because `null rd`
      inub && (findMax' ld) <  lb && (findMax' t) == (findMax' bd) && (findMax' ld) < (findMin' bd)

  | (nnf ld) && (nnf bd) && (nnf rd) = traceX "H" $
      (findMax' ld) <  lb && ub < (findMin' rd) && inlb && inub
  | otherwise = error "[ERROR] rbTester: Can't be"

  where
    (ld,bd,rd) = roughBound lb ub t
    inlb = inlbf lb bd
    inub = inubf ub bd
    (lb,ub) = if (abs lbx) <= (abs ubx) then (abs lbx,abs ubx) else (abs ubx,abs lbx)

nf  = I.null
nnf = not . I.null

inlbf lb t = notBin t || (matchf lb t || lb <= (findMin' t))
inubf ub t = notBin t || (matchf ub t || (findMax' t) <= ub)

notBin Bin {} = False
notBin _ = True

seqTesterSub :: [(Int,a)] -> Bool
seqTesterSub [] = True
seqTesterSub [_] = True
seqTesterSub ~((a,_):(b,x):rest) = a < b && seqTesterSub ((b,x):rest)

seqTester :: (IntMap Key -> [(Int,Key)]) -> [Int] -> Bool
seqTester f aList = seqTesterSub list && length list == I.size aMap
  where
    -- aMap :: IntMap a
    aMap = double aList
    -- list :: [(Int,a)]
    list = f aMap

binTester :: [Int] -> Bool
binTester aList = go aMap
  where
    aMap = double aList
    go (Bin p m l r) = (popCount m == 1) && (p == 0 || (p B..&. m) == zeroBits ) && go l && go r
    go (Tip _ _) = True
    go ~Nil = True




fixer f lbx ubx list = f lb ub t
  where
    (lb,ub) = if lbx < ubx then (lbx, ubx) else (ubx, lbx)
    t = double list

pFixer f lbx ubx list = f lb ub t
  where
    (lb,ub) = if (abs lbx) < (abs ubx) then (abs lbx, abs ubx) else (abs ubx, abs lbx)
    t = double . absList $ list

tester f vf lbx ubx list =
  f lb ub t == vf lb ub t
  where
    (lb,ub) = if lbx < ubx then (lbx, ubx) else (ubx, lbx)
    t = double list

pTester f vf lbx ubx list =
  f lb ub t == vf lb ub t
  where
    (lb,ub) = if (abs lbx) < (abs ubx) then (abs lbx, abs ubx) else (abs ubx, abs lbx)
    t = double . absList $ list

tView f vf lbx ubx list = do
  print $ f lb ub t
  print $ vf lb ub t
  where
    (lb,ub) = if (abs lbx) < (abs ubx) then (abs lbx, abs ubx) else (abs ubx, abs lbx)
    t = double list

prop_RB =
  quickCheckWith stdArgs { maxSuccess = 100000 }
    ((\lb ub list -> rbTester lb ub (double list)) :: Int -> Int -> [Int] -> Bool)

prop_pRB =
  quickCheckWith stdArgs { maxSuccess = 100000 }
    ((\lb ub list -> rbTester lb ub (double . absList $ list)) :: Int -> Int -> [Int] -> Bool)

prop_F f vf =
  quickCheckWith stdArgs { maxSuccess = 1000000 }
    (tester f vf :: Int -> Int -> [Int] -> Bool)

prop_pF f vf =
  quickCheckWith stdArgs { maxSuccess = 1000000 }
    (pTester f vf :: Int -> Int -> [Int] -> Bool)

prop_pB = prop_pF bounded (toListf boundedV)

-- For use like: prop_pB bound (toListf takeBoundedB)
toListf f lb ub t = toList $ f lb ub t

prop_G =
  quickCheckWith stdArgs { maxSuccess = 100000 }
    (seqTester grabAll :: [Int] -> Bool)

prop_BPM =
  quickCheckWith stdArgs { maxSuccess = 100000 }
    (binTester :: [Int] -> Bool)
