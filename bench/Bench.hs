{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Criterion.Types
import Data.Maybe
import Data.Either
import Control.Monad

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


myConfig = defaultConfig {timeLimit=20.0}

gens = [mkStdGen x | x<- [1..10000]]
randomIntList = randomRs (0,999999) (mkStdGen 0) :: [Int]
randomIntLists = map (randomRs (0,999999)) gens :: [[Int]]

lRandomIntList = take 10000 randomIntList
lRandomIntLists len = take 1000 $ map (\l -> take ((quot (head l) len) + 5) l) randomIntLists :: [[Int]]

lrils1000 = lRandomIntLists 1000
lrils100  = lRandomIntLists 10000
lrils10   = lRandomIntLists 100000

mBound :: [[Int]] -> [[(Int,Int)]]
mBound = map (\l -> pFixer bound (l !! 0) (l !! 1) (drop 2 l))
mCombined :: [[Int]] -> [[(Int,Int)]]
mCombined = map (\l -> I.toList $ pFixer boundedV (l !! 0) (l !! 1) (drop 2 l))
mFilter :: [[Int]] -> [[Int]]
mFilter = map (\l -> filterA (l !! 0) (l !! 1) (drop 2 l))

filterA lb ub list = reverse $ go [] list
  where
    go l [] = l
    go [] [a] = [a]
    go l (h:rest)
      | h < lb = go [h] rest
      | h > ub = h:l
      | otherwise = go (h:l) rest

main = lrils10 `deepseq` lrils100 `deepseq` lrils1000 `deepseq` defaultMainWith myConfig [
  bgroup "Test custom function"
    [ bench "Custom-10"   $ nf mBound lrils10
    , bench "Combined-10" $ nf mCombined lrils10
    , bench "List-10"     $ nf mCombined lrils10
    , bench "Custom-100"   $ nf mBound lrils100
    , bench "Combined-100" $ nf mCombined lrils100
    , bench "List-100"     $ nf mCombined lrils100
    , bench "Custom-1000"   $ nf mBound lrils1000
    , bench "Combined-1000" $ nf mCombined lrils1000
    , bench "List-1000"     $ nf mCombined lrils1000
    ]
  ]
