{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common where

import           Control.DeepSeq
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Cuckoo
import qualified Data.HashTable.ST.Linear


instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Cuckoo.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Linear.HashTable s k v) where
  rnf x = seq x ()
