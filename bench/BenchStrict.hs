{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Criterion.Types
import Data.Maybe
import Data.Either
import Control.Monad

import Data.IntMap.Hack.Tool
import qualified Data.IntMap.Bound as O
import qualified Data.IntMap.BoundN as N
import Data.IntMap.Bound.Verifier hiding (nf)
import qualified Data.IntMap.Internal as I

import qualified Data.ByteString.Char8 as BC

import qualified Data.ByteString.Base64.URL as DB
import qualified Codec.Binary.Base64Url as CB

import Control.DeepSeq
import System.IO
import System.Random
import Debug.Trace


myConfig = defaultConfig {timeLimit=60.0}

gens = [mkStdGen x | x<- [1..10000]]
randomIntList = randomRs (0,999999) (mkStdGen 0) :: [Int]
randomIntLists = map (randomRs (0,999999)) gens :: [[Int]]

lRandomIntList = take 10000 randomIntList
lRandomIntLists len num = take 10000 $ drop (10000*num) $ map (\l -> take ((quot (head l) len) + 5) l) randomIntLists :: [[Int]]

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

mBound :: [(Int,Int,I.IntMap Int)] -> [[(Int,Int)]]
mBound = map (\(lb,ub,m) -> O.bounded lb ub m)

mBoundN :: [(Int,Int,I.IntMap Int)] -> [[(Int,Int)]]
mBoundN = map (\(lb,ub,m) -> N.bounded lb ub m)


main = --lrils10 `deepseq` lrils11 `deepseq` lrils12 `deepseq` lrils13 `deepseq`
       --lrils100 `deepseq` lrils101 `deepseq` lrils102 `deepseq` lrils103 `deepseq`
       --lrils1000 `deepseq` lrils1001 `deepseq` lrils1002 `deepseq` lrils1003 `deepseq`
       lrims10 `deepseq` lrims11 `deepseq` lrims12 `deepseq` lrims13 `deepseq`
       lrims100 `deepseq` lrims101 `deepseq` lrims102 `deepseq` lrims103 `deepseq`
       lrims1000 `deepseq` lrims1001 `deepseq` lrims1002 `deepseq` lrims1003 `deepseq`
         defaultMainWith myConfig [
  bgroup "Test strict function ~100"
    [ bench "Lazy-10"   $ nf mBound lrims10
    , bench "Strict-10" $ nf mBoundN lrims10
    -- , bench "Lazy-11"   $ nf mBound lrims11
    -- , bench "Strict-11" $ nf mBoundN lrims11
    -- , bench "Lazy-12"   $ nf mBound lrims12
    -- , bench "Strict-12" $ nf mBoundN lrims12
    -- , bench "Lazy-13"   $ nf mBound lrims13
    -- , bench "Strict-13" $ nf mBoundN lrims13
    ]
  , bgroup "Test strict function ~1000"
    [ bench "Lazy-100"   $ nf mBound lrims100
    , bench "Strict-100" $ nf mBoundN lrims100
    -- , bench "Lazy-101"   $ nf mBound lrims101
    -- , bench "Strict-101" $ nf mBoundN lrims101
    -- , bench "Lazy-102"   $ nf mBound lrims102
    -- , bench "Strict-102" $ nf mBoundN lrims102
    -- , bench "Lazy-103"   $ nf mBound lrims103
    -- , bench "Strict-103" $ nf mBoundN lrims103
    ]
  , bgroup "Test strict function ~10000"
    [ bench "Lazy-1000"   $ nf mBound lrims1000
    , bench "Strict-1000" $ nf mBoundN lrims1000
    -- , bench "Lazy-1001"   $ nf mBound lrims1001
    -- , bench "Strict-1001" $ nf mBoundN lrims1001
    -- , bench "Lazy-1002"   $ nf mBound lrims1002
    -- , bench "Strict-1002" $ nf mBoundN lrims1002
    -- , bench "Lazy-1003"   $ nf mBound lrims1003
    -- , bench "Strict-1003" $ nf mBoundN lrims1003
    ]
  ]
