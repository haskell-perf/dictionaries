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
import           System.Random

data Insert = forall f. NFData (f Int) => Insert String (Int -> f Int)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "InsertInt"
        (conses
           [ Insert "Data.Map.Lazy" insertMapLazy
           , Insert "Data.Map.Strict" insertMapStrict
           , Insert "Data.HashMap.Lazy" insertHashMapLazy
           , Insert "Data.HashMap.Strict" insertHashMapStrict
           , Insert "Data.IntMap.Lazy" insertIntMapLazy
           , Insert "Data.IntMap.Strict" insertIntMapStrict
           ])
    ]
  where
    conses funcs =
      [ env (let !elems =
                   force (zip (randoms (mkStdGen 0) :: [Int])
                              [1 :: Int .. i])
             in pure elems)
            (\_ -> bench (title ++ " 0.." ++ show i) $ nf func i)
      | i <- [10, 1000, 10000]
      , Insert title func <- funcs
      ]

insertMapLazy :: Int -> Data.Map.Lazy.Map Int Int
insertMapLazy n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.Map.Lazy.insert n n acc)

insertMapStrict :: Int -> Data.Map.Strict.Map Int Int
insertMapStrict n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.Map.Strict.insert n n acc)

insertHashMapLazy :: Int -> Data.HashMap.Lazy.HashMap Int Int
insertHashMapLazy n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.HashMap.Lazy.insert n n acc)

insertHashMapStrict :: Int -> Data.HashMap.Strict.HashMap Int Int
insertHashMapStrict n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.HashMap.Strict.insert n n acc)

insertIntMapLazy :: Int -> Data.IntMap.Lazy.IntMap  Int
insertIntMapLazy n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.IntMap.Lazy.insert n n acc)

insertIntMapStrict :: Int -> Data.IntMap.Strict.IntMap  Int
insertIntMapStrict n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.IntMap.Strict.insert n n acc)
