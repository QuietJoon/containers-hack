{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Criterion.Types
import Data.Maybe
import Data.Either
import Control.Monad

import Data.IntMap.Hack.Tool
import Data.IntMap.Bound
import Data.IntMap.Bound.Verifier hiding (nf)
import qualified Data.IntMap.Internal as I

import qualified Data.ByteString.Char8 as BC

import qualified Data.ByteString.Base64.URL as DB
import qualified Codec.Binary.Base64Url as CB

import Control.DeepSeq
import System.IO
import System.Random
import Debug.Trace


myConfig = defaultConfig {timeLimit=120.0}

gens = [mkStdGen x | x<- [1..10000]]
randomIntList = randomRs (0,999999) (mkStdGen 0) :: [Int]
randomIntLists = map (randomRs (0,999999)) gens :: [[Int]]

lRandomIntList = take 10000 randomIntList
lRandomIntLists len num = take 10000 $ drop (10000*num) $ map (\l -> take ((quot (head l) len) + 5) l) randomIntLists :: [[Int]]

lrims1000 = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 1000 0 :: [(Int,Int,I.IntMap Int)]
lrims1001 = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 1000 1 :: [(Int,Int,I.IntMap Int)]
lrims1002 = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 1000 2 :: [(Int,Int,I.IntMap Int)]
lrims1003 = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 1000 3 :: [(Int,Int,I.IntMap Int)]
lrims100  = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 10000 0 :: [(Int,Int,I.IntMap Int)]
lrims101  = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 10000 1 :: [(Int,Int,I.IntMap Int)]
lrims102  = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 10000 2 :: [(Int,Int,I.IntMap Int)]
lrims103  = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 10000 3 :: [(Int,Int,I.IntMap Int)]
lrims10   = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 100000 0 :: [(Int,Int,I.IntMap Int)]
lrims11   = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 100000 1 :: [(Int,Int,I.IntMap Int)]
lrims12   = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 100000 2 :: [(Int,Int,I.IntMap Int)]
lrims13   = map (\(lb:ub:rest) -> (lb,ub, (double . absList $ rest))) $ lRandomIntLists 100000 3 :: [(Int,Int,I.IntMap Int)]

lrils1000 = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 1000 0
lrils1001 = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 1000 1
lrils1002 = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 1000 2
lrils1003 = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 1000 3
lrils100  = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 10000 0
lrils101  = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 10000 1
lrils102  = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 10000 2
lrils103  = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 10000 3
lrils10   = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 100000 0
lrils11   = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 100000 1
lrils12   = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 100000 2
lrils13   = map (\(lb:ub:rest) -> (lb,ub, (absList $ rest))) $ lRandomIntLists 100000 3


mBound :: [(Int,Int,I.IntMap Int)] -> [[(Int,Int)]]
mBound = map (\(lb,ub,m) -> boundedOf lb ub m)

mCombined :: [(Int,Int,I.IntMap Int)] -> [[(Int,Int)]]
mCombined = map (\(lb,ub,m) -> I.toList $ boundedV lb ub m)

mFilter :: [(Int,Int,[Int])] -> [[Int]]
mFilter = map (\(lb,ub,l) -> filterA lb ub l)

filterA lb ub list = reverse $ go [] list
  where
    go l [] = l
    go [] [a] = [a]
    go l (h:rest)
      | h < lb = go [h] rest
      | h > ub = h:l
      | otherwise = go (h:l) rest

main = lrils10 `deepseq` lrils11 `deepseq` lrils12 `deepseq` lrils13 `deepseq`
       lrils100 `deepseq` lrils101 `deepseq` lrils102 `deepseq` lrils103 `deepseq`
       lrils1000 `deepseq` lrils1001 `deepseq` lrils1002 `deepseq` lrils1003 `deepseq`
       lrims10 `deepseq` lrims11 `deepseq` lrims12 `deepseq` lrims13 `deepseq`
       lrims100 `deepseq` lrims101 `deepseq` lrims102 `deepseq` lrims103 `deepseq`
       lrims1000 `deepseq` lrims1001 `deepseq` lrims1002 `deepseq` lrims1003 `deepseq`
         defaultMainWith myConfig [
    bgroup "Test custom function ~100"
    [ bench "Custom-10"   $ nf mBound lrims10
    , bench "Combined-10" $ nf mCombined lrims10
    , bench "List-10"     $ nf mFilter lrils10
    ]
  , bgroup "Test custom function ~1000"
    [ bench "Custom-100"   $ nf mBound lrims100
    , bench "Combined-100" $ nf mCombined lrims100
    , bench "List-100"     $ nf mFilter lrils100
    ]
  , bgroup "Test custom function ~10000"
    [ bench "Custom-1000"   $ nf mBound lrims1000
    , bench "Combined-1000" $ nf mCombined lrims1000
    , bench "List-1000"     $ nf mFilter lrils1000
    ]
  ]
