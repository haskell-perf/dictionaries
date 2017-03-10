{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.HashMap.Lazy
import qualified Data.HashMap.Strict
import qualified Data.IntMap.Lazy
import qualified Data.IntMap.Strict
import qualified Data.Map.Lazy
import qualified Data.Map.Strict

data Conser = forall f. NFData (f Int) => Conser String (Int -> f Int)
data Replicator = forall f. NFData (f Int) => Replicator String (Int -> Int -> f Int)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Consing"
        (conses
           [ Conser "Data.List" conslist
           , Conser "Data.Vector" consvector
           , Conser "Data.Vector.Unboxed" consuvector
           , Conser "Data.Sequence" consseq
           ])
    , bgroup
        "Replicate"
        (replicators
           [ Replicator "Data.List" L.replicate
           , Replicator "Data.Vector" V.replicate
           , Replicator "Data.Vector.Unboxed" UV.replicate
           , Replicator "Data.Sequence" S.replicate
           ])
    ]
  where
    conses funcs =
      [ bench (title ++ " 0.." ++ show i) $ nf func i
      | i <- [10, 1000, 10000]
      , Conser title func <- funcs
      ]
    replicators funcs =
      [ bench (title ++ " " ++ show i) $ nf (\(x,y) -> func x y) (i,1234)
      | i <- [10, 1000, 10000]
      , Replicator title func <- funcs
      ]

conslist :: Int -> [Int]
conslist n0 = go n0 []
  where go 0 acc = acc
        go n !acc = go (n - 1) (n : acc)

consvector :: Int -> V.Vector Int
consvector n0 = go n0 V.empty
  where go 0 acc = acc
        go n !acc = go (n - 1) (V.cons n acc)

consuvector :: Int -> UV.Vector Int
consuvector n0 = go n0 UV.empty
  where go 0 acc = acc
        go n !acc = go (n - 1) (UV.cons n acc)

consseq :: Int -> S.Seq Int
consseq n0 = go n0 S.empty
  where go 0 acc = acc
        go n !acc = go (n - 1) (n S.<| acc)
