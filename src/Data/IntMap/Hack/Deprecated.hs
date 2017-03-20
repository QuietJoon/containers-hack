module Data.IntMap.Hack.Deprecated where


import Data.IntMap.Internal as I


grabAll :: IntMap a -> [(Key,a)]
grabAll = \t ->
  case t of
    (Bin _ m l r) | m < 0 -> go r (go l [])
    _ -> go t []
  where
    go (Bin _ _ l r) aList = go l (go r aList)
    go (Tip k v) aList = (k,v):aList
    go Nil aList = aList
