module Data.IntMap.Hack.Deprecated where


import Data.IntMap.Bound.Base

import Prelude as P

import Data.IntMap.Internal as I

import Debug.Shortcut


grabAll = \t ->
  case t of
    (Bin p m l r) | m < 0 -> go r (go l [])
    _ -> go t []
  -- case t of 
  where
    go (Bin _ _ l r) aList = go l (go r aList)
    go (Tip k v) aList = (k,v):aList
    go Nil aList = aList
