{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Criterion.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashMap.Lazy
import qualified Data.HashMap.Strict
import qualified Data.HashTable.IO
import qualified Data.HashTable.Class
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Cuckoo
import qualified Data.HashTable.ST.Linear
import qualified Data.IntMap.Lazy
import qualified Data.IntMap.Strict
import qualified Data.Judy
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import qualified Data.Trie
import           System.Directory
import           System.Random

data InsertInt = forall f. NFData (f Int) => InsertInt String (Int -> f Int)

data InsertIntIO = forall d. NFData d => InsertIntIO String (IO d) (d -> Int -> IO d)

data FromListBS =
  forall f. NFData (f Int) =>
            FromListBS String
                     ([(ByteString,Int)] -> f Int)

data Intersection = forall f. NFData (f Int) =>
     Intersection String ([(Int,Int)] -> f Int) (f Int -> f Int -> f Int)

data IntersectionIO = forall d. NFData d =>
     IntersectionIO String ([(Int,Int)] -> IO d) (d -> d -> IO d)

data Lookup =
  forall f. (NFData (f Int)) =>
            Lookup String
                   ([(Int, Int)] -> f Int)
                   (Int -> f Int ->  (Maybe Int))

data LookupIO = 
  forall d. NFData d => LookupIO String ([(Int,Int)] -> IO d) (d -> Int -> IO (Maybe Int))

-- | TODO: We need a proper deepseq. But Trie seems to perform awfully anyway so far, anyway.
instance NFData (Data.Trie.Trie a) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Cuckoo.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Linear.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.Judy.JudyL v) where
  rnf x = seq x ()

main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    [ bgroup
        "Insert Int (Randomized)"
        (insertInts
           [ InsertInt "Data.Map.Lazy" insertMapLazy
           , InsertInt "Data.Map.Strict" insertMapStrict
           , InsertInt "Data.HashMap.Lazy" insertHashMapLazy
           , InsertInt "Data.HashMap.Strict" insertHashMapStrict
           , InsertInt "Data.IntMap.Lazy" insertIntMapLazy
           , InsertInt "Data.IntMap.Strict" insertIntMapStrict
           ])
    , bgroup
        "IO Insert Int (Randomized)"
        (insertIntsIO
           [ InsertIntIO "Data.HashTable.IO.BasicHashTable" Data.HashTable.IO.new insertHashTableIOBasic
           , InsertIntIO "Data.HashTable.IO.LinearHashTable" Data.HashTable.IO.new insertHashTableIOLinear
           , InsertIntIO "Data.HashTable.IO.CuckooHashTable" Data.HashTable.IO.new insertHashTableIOCuckoo
           , InsertIntIO "Data.Judy" Data.Judy.new insertJudy
           ])
    , bgroup
        "Intersection (Randomized)"
        (intersection
           [ Intersection "Data.Map.Lazy" Data.Map.Lazy.fromList Data.Map.Lazy.intersection
           , Intersection "Data.Map.Strict" Data.Map.Strict.fromList Data.Map.Strict.intersection
           , Intersection "Data.HashMap.Lazy" Data.HashMap.Lazy.fromList Data.HashMap.Lazy.intersection
           , Intersection "Data.HashMap.Strict" Data.HashMap.Strict.fromList Data.HashMap.Strict.intersection
           , Intersection "Data.IntMap.Lazy" Data.IntMap.Lazy.fromList Data.IntMap.Lazy.intersection
           , Intersection "Data.IntMap.Strict" Data.IntMap.Strict.fromList Data.IntMap.Strict.intersection
           ])
    , bgroup
        "IO Intersection (Randomized)"
        (intersectionIO
           [ IntersectionIO "Data.HashTable.IO.BasicHashTable" Data.HashTable.IO.fromList intersectionHashTableIOBasic
           , IntersectionIO "Data.HashTable.IO.LinearHashTable" Data.HashTable.IO.fromList intersectionHashTableIOLinear
           , IntersectionIO "Data.HashTable.IO.CuckooHashTable" Data.HashTable.IO.fromList intersectionHashTableIOCuckoo
           , IntersectionIO "Data.Judy" judyFromList intersectionJudy
           ])
    , bgroup
        "Lookup Int (Randomized)"
        (lookupRandomized
           [ Lookup "Data.Map.Lazy" Data.Map.Lazy.fromList Data.Map.Lazy.lookup
           , Lookup
               "Data.Map.Strict"
               Data.Map.Strict.fromList
               Data.Map.Strict.lookup
           , Lookup
               "Data.HashMap.Lazy"
               Data.HashMap.Lazy.fromList
               Data.HashMap.Lazy.lookup
           , Lookup
               "Data.HashMap.Strict"
               Data.HashMap.Strict.fromList
               Data.HashMap.Strict.lookup
           , Lookup
               "Data.IntMap.Lazy"
               Data.IntMap.Lazy.fromList
               Data.IntMap.Lazy.lookup
           , Lookup
               "Data.IntMap.Strict"
               Data.IntMap.Strict.fromList
               Data.IntMap.Strict.lookup
           ])
    , bgroup
        "IO Lookup Int (Randomized)"
        (lookupRandomizedIO
            [ LookupIO "Data.Judy" judyFromList judyLookup
            , LookupIO "Data.HashTable.IO.BasicHashTable"
                (Data.HashTable.IO.fromList :: [(Int,Int)] -> IO (Data.HashTable.IO.BasicHashTable Int Int))
                Data.HashTable.IO.lookup
            , LookupIO "Data.HashTable.IO.LinearHashTable"
                (Data.HashTable.IO.fromList :: [(Int,Int)] -> IO (Data.HashTable.IO.LinearHashTable Int Int))
                Data.HashTable.IO.lookup
            , LookupIO "Data.HashTable.IO.CuckooHashTable"
                (Data.HashTable.IO.fromList :: [(Int,Int)] -> IO (Data.HashTable.IO.CuckooHashTable Int Int))
                 Data.HashTable.IO.lookup
            ])
    , bgroup
        "FromList ByteString (Monotonic)"
        (insertBSMonotonic
           [ FromListBS "Data.Map.Lazy" Data.Map.Lazy.fromList
           , FromListBS "Data.Map.Strict" Data.Map.Strict.fromList
           , FromListBS "Data.HashMap.Lazy" Data.HashMap.Lazy.fromList
           , FromListBS "Data.HashMap.Strict" Data.HashMap.Strict.fromList
           , FromListBS "Data.Trie" Data.Trie.fromList
           ])
    , bgroup
        "FromList ByteString (Randomized)"
        (insertBSRandomized
           [ FromListBS "Data.Map.Lazy" Data.Map.Lazy.fromList
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
        (\_ -> bench (title ++ ":" ++ show i) $ nf func i)
      | i <- [10, 100, 1000, 10000]
      , InsertInt title func <- funcs
      ]
    insertIntsIO funcs =
      [ env init (\init -> bench (title ++ ":" ++ show i) $ nfIO (func init i))
      | i <- [10, 100, 1000, 10000]
      , InsertIntIO title init func <- funcs
      ]
    intersection funcs =
      [ env
        (let !args =
               force ( build (zip (randoms (mkStdGen 0) :: [Int]) [1 :: Int .. i])
                     , build (zip (randoms (mkStdGen 1) :: [Int]) [1 :: Int .. i])
                     )
         in  pure args)
        (\ args -> bench (title ++ ":" ++ show i) $ nf (uncurry intersect) args)
      | i <- [10, 100, 1000, 10000]
      , Intersection title build intersect <- funcs
      ]
    intersectionIO funcs =
      [ env
        (let !args =
               force ( build (zip (randoms (mkStdGen 0) :: [Int]) [1 :: Int .. i])
                     , build (zip (randoms (mkStdGen 1) :: [Int]) [1 :: Int .. i])
                     )
         in  pure args)
        (\ (xs,ys) -> bench (title ++ ":" ++ show i) $ nfIO (intersect xs ys))
      | i <- [10, 100, 1000, 10000]
      , IntersectionIO title build intersect <- funcs
      ]
    insertBSRandomized funcs =
      [ env
        (let !elems =
               force
                 (map
                    (first (S8.pack . show))
                    (take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])))
         in pure elems)
        (\elems -> bench (title ++ ":" ++ show i) $ nf func elems)
      | i <- [10, 100, 1000, 10000]
      , FromListBS title func <- funcs
      ]
    lookupRandomized funcs =
      [ env
        (let !elems =
               force
                 (fromList (take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])))
         in pure elems)
        (\elems -> bench (title ++ ":" ++ show i) $ nf (flip func elems) (div i 2))
      | i <- [10, 100, 1000, 10000]
      , Lookup title fromList func <- funcs
      ]
    lookupRandomizedIO funcs =
      [ env
        (let !elems =
               force <$> 
                 (fromList (take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])))
         in elems)
        (\elems -> bench (title ++ ":" ++ show i) $ nfIO (func elems (div i 2)))
      | i <- [10, 100, 1000, 10000]
      , LookupIO title fromList func <- funcs
      ]
    insertBSMonotonic funcs =
      [ env
        (let !elems =
               force (map (first (S8.pack . show)) (take i (zip [1 :: Int ..] [1 ..])))
         in pure elems)
        (\elems -> bench (title ++ ":" ++ show i) $ nf func elems)
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

insertHashTableIO :: Data.HashTable.Class.HashTable ht => ht s Int Int -> Int -> IO (ht s Int Int)
insertHashTableIO ht n0 = do
  mapM_ (\n -> Data.HashTable.IO.insert ht n n) [1..n0]
  return ht

insertHashTableIOBasic :: Data.HashTable.IO.BasicHashTable Int Int -> Int -> IO (Data.HashTable.IO.BasicHashTable Int Int)
insertHashTableIOBasic = insertHashTableIO

insertHashTableIOCuckoo :: Data.HashTable.IO.CuckooHashTable Int Int -> Int -> IO (Data.HashTable.IO.CuckooHashTable Int Int)
insertHashTableIOCuckoo = insertHashTableIO

insertHashTableIOLinear :: Data.HashTable.IO.LinearHashTable Int Int -> Int -> IO (Data.HashTable.IO.LinearHashTable Int Int)
insertHashTableIOLinear = insertHashTableIO

insertJudy :: Data.Judy.JudyL Int -> Int -> IO (Data.Judy.JudyL Int)
insertJudy j n0 = do
  mapM_ (\n -> Data.Judy.insert (fromIntegral n) n j) [1 .. n0]
  return j

judyFromList :: [(Int,Int)] -> IO (Data.Judy.JudyL Int)
judyFromList xs = do
  j <- Data.Judy.new
  mapM_ (\(k,v) -> Data.Judy.insert (fromIntegral k) v j) xs
  return j

judyLookup :: Data.Judy.JudyL Int -> Int -> IO (Maybe Int)
judyLookup j k = Data.Judy.lookup (fromIntegral k) j

intersectionJudy :: Data.Judy.JudyL Int -> Data.Judy.JudyL Int -> IO (Data.Judy.JudyL Int)
intersectionJudy ij0 ij1 = do
  j <- Data.Judy.new
  j0 <- Data.Judy.unsafeFreeze ij0
  j1 <- Data.Judy.unsafeFreeze ij1
  j0Kvs <- Data.Judy.toList j0
  j1Kvs <- Data.Judy.toList j1
  mapM_ (\(k,v) -> Data.Judy.insert k v j) j0Kvs
  mapM_ (\(k,v) -> Data.Judy.insert k v j) j1Kvs
  return j

intersectionHashTableIO :: Data.HashTable.Class.HashTable ht => ht s Int Int -> ht s Int Int -> IO (ht s Int Int)
intersectionHashTableIO ht0 ht1 = do
  ht <- Data.HashTable.IO.new
  Data.HashTable.IO.mapM_ (\(k,v) -> Data.HashTable.IO.insert ht k v) ht0
  Data.HashTable.IO.mapM_ (\(k,v) -> Data.HashTable.IO.insert ht k v) ht1
  return ht

intersectionHashTableIOBasic :: Data.HashTable.IO.BasicHashTable Int Int
  -> Data.HashTable.IO.BasicHashTable Int Int
  -> IO (Data.HashTable.IO.BasicHashTable Int Int )
intersectionHashTableIOBasic = intersectionHashTableIO

intersectionHashTableIOCuckoo :: Data.HashTable.IO.CuckooHashTable Int Int
  -> Data.HashTable.IO.CuckooHashTable Int Int
  -> IO (Data.HashTable.IO.CuckooHashTable Int Int)
intersectionHashTableIOCuckoo = intersectionHashTableIO

intersectionHashTableIOLinear :: Data.HashTable.IO.LinearHashTable Int Int
  -> Data.HashTable.IO.LinearHashTable Int Int
  -> IO (Data.HashTable.IO.LinearHashTable Int Int)
intersectionHashTableIOLinear = intersectionHashTableIO
