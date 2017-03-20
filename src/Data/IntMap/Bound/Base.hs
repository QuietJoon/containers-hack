module Data.IntMap.Bound.Base where


import Data.IntMap.Internal as I


import Data.Maybe

-- Test P & M to verify positive/negative range

pnTester :: IntMap a -> Ordering
pnTester = \t ->
  case t of
    (Bin p m _ _) ->
      case () of
        _
          | m < 0 -> EQ
          | p < 0 -> LT
          | otherwise -> GT
    (Tip k _) | k < 0 -> LT
    (Tip k _) -> GT
    Nil -> EQ

lookupGT' k t = fst . fromJust $ lookupGT k t
lookupGE' k t = fst . fromJust $ lookupGE k t
lookupLT' k t = fst . fromJust $ lookupLT k t
lookupLE' k t = fst . fromJust $ lookupLE k t


findMinX op k t = (not . I.null $ t) && (op (fst (findMin t)) k)
findMaxX op k t = (not . I.null $ t) && (op (fst (findMax t)) k)


nomatchf k (Bin p m _ _) = nomatch k p m
nomatchf k (Tip tk _) = k /= tk
nomatchf _ _ = True

matchf k t = not $ nomatchf k t

{-
matchf k (Bin p m _ _) = match k p m
matchf k (Tip tk _) = k == tk
matchf _ _ = False
-}

left (Bin _ _ l _) = l
right (Bin _ _ _ r) = r

zerof k (Bin _ m _ _) = zero k m
zerof k _ = False


findMin' t = fst $ findMin t
findMax' t = fst $ findMax t
