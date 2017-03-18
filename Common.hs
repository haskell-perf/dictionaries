module Common where

import           Control.DeepSeq
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Cuckoo
import qualified Data.HashTable.ST.Linear
import qualified Data.Judy


instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Cuckoo.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.HashTable.ST.Linear.HashTable s k v) where
  rnf x = seq x ()

instance NFData (Data.Judy.JudyL v) where
  rnf x = seq x ()

judyFromList :: [(Int,Int)] -> IO (Data.Judy.JudyL Int)
judyFromList xs = do
  j <- Data.Judy.new
  mapM_ (\(k,v) -> Data.Judy.insert (fromIntegral k) v j) xs
  return j
