module Data.IntMap.Hack.Analysis where


import Data.IntMap.Internal as I


showPTree t = go t 0
  where
    go (Bin p m r l) n
      =  go l (n+1)
      ++ spacing n ++ "P: " ++ show p ++ "\n"
      ++ spacing n ++ "M: " ++ show m ++ "\n"
      ++ go r (n+1)
    go (Tip k v) n = spacing n ++ "K: " ++ show k ++ "\n"
    go Nil n = spacing n ++ "Nil" ++ "\n"

showKTree t = go t 0
  where
    go (Bin p m r l) n
      =  go l (n+1)
      ++ go r (n+1)
    go (Tip k v) n = spacing n ++ "K: " ++ show k ++ "\n"
    go Nil n = spacing n ++ "Nil" ++ "\n"

printPTree = putStrLn . showPTree
printKTree = putStrLn . showKTree

{-# INLINE spacing #-}
spacing n = replicate (n*2) ' '
