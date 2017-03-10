{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.Arrow
import           Control.DeepSeq
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashMap.Lazy
import qualified Data.HashMap.Strict
import qualified Data.IntMap.Lazy
import qualified Data.IntMap.Strict
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import qualified Data.Trie
import           System.Random

data InsertInt = forall f. NFData (f Int) => InsertInt String (Int -> f Int)

data FromListBS =
  forall f. NFData (f Int) =>
            FromListBS String
                     ([(ByteString,Int)] -> f Int)

-- | TODO: We need a proper deepseq. But Trie seems to perform awfully anyway so far, anyway.
instance NFData (Data.Trie.Trie a) where
  rnf x = seq x ()

main :: IO ()
main =
  defaultMain
    [ bgroup
        "InsertInt"
        (insertInts
           [ InsertInt "Data.Map.Lazy" insertMapLazy
           , InsertInt "Data.Map.Strict" insertMapStrict
           , InsertInt "Data.HashMap.Lazy" insertHashMapLazy
           , InsertInt "Data.HashMap.Strict" insertHashMapStrict
           , InsertInt "Data.IntMap.Lazy" insertIntMapLazy
           , InsertInt "Data.IntMap.Strict" insertIntMapStrict
           ])
    , bgroup
        "FromListByteStringMonotonic"
        (insertBSMonotonic
           [ FromListBS "Data.Map.Lazy"Data.Map.Lazy.fromList
           , FromListBS "Data.Map.Strict" Data.Map.Strict.fromList
           , FromListBS "Data.HashMap.Lazy" Data.HashMap.Lazy.fromList
           , FromListBS "Data.HashMap.Strict" Data.HashMap.Strict.fromList
           , FromListBS "Data.Trie" Data.Trie.fromList
           ])
    ,  bgroup
         "FromListByteStringRandomized"
         (insertBSRandomized
            [ FromListBS "Data.Map.Lazy"Data.Map.Lazy.fromList
            , FromListBS "Data.Map.Strict" Data.Map.Strict.fromList
            , FromListBS "Data.HashMap.Lazy" Data.HashMap.Lazy.fromList
            , FromListBS "Data.HashMap.Strict" Data.HashMap.Strict.fromList
            , FromListBS "Data.Trie" Data.Trie.fromList
            ])
    ]
  where
    insertInts funcs =
      [ env
        (let !elems =
               force (zip (randoms (mkStdGen 0) :: [Int]) [1 :: Int .. i])
         in pure elems)
        (\_ -> bench (title ++ " 0.." ++ show i) $ nf func i)
      | i <- [10, 1000, 10000]
      , InsertInt title func <- funcs
      ]
    insertBSRandomized funcs =
      [ env
        (let !elems =
               force
                 (map (first (S8.pack . show)) (take i (zip (randoms (mkStdGen 0) :: [Int]) [1..])))
         in pure elems)
        (\elems -> bench (title ++ " 0.." ++ show i) $ nf func elems)
      | i <- [10, 1000, 10000]
      , FromListBS title func <- funcs
      ]
    insertBSMonotonic funcs =
      [ env
        (let !elems =
               force
                 (map (first (S8.pack . show)) (take i (zip [1..] [1..])))
         in pure elems)
        (\elems -> bench (title ++ " 0.." ++ show i) $ nf func elems)
      | i <- [10000]
      , FromListBS title func <- funcs
      ]

--------------------------------------------------------------------------------
-- Insert Int

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

insertIntMapLazy :: Int -> Data.IntMap.Lazy.IntMap Int
insertIntMapLazy n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.IntMap.Lazy.insert n n acc)

insertIntMapStrict :: Int -> Data.IntMap.Strict.IntMap Int
insertIntMapStrict n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.IntMap.Strict.insert n n acc)
