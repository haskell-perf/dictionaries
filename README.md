# dictionaries

Benchmarks for dictionary data structures: hash tables, maps, tries, etc.

## Running

For all benchmarks:

    $ stack bench :space

For just space:

    $ stack bench :space

For just time:

    $ stack bench :time

## Insert Int keys


|Case|                                              Bytes|    GCs|
|---|---|---|
|Data.Map.Strict.insert mempty                        |64      |0  |
|Data.Map.Lazy.insert mempty                          |64      |0  |
|Data.HashMap.Strict.insert mempty                    |64      |0  |
|Data.HashMap.Lazy.insert mempty                      |48      |0  |


## From List Int keys


|Case|                                              Bytes|    GCs|
|---|---|---|
|Data.Map.Strict.fromList     (1 million)|  1,016,187,152  |1,942  |
|Data.Map.Lazy.fromList       (1 million)|  1,016,187,152  |1,942  |
|Data.IntMap.Strict.fromList  (1 million)|    776,852,648  |1,489  |
|Data.IntMap.Lazy.fromList    (1 million)|    776,852,648  |1,489  |
|Data.HashMap.Strict.fromList (1 million)|    161,155,384    |314  |
|Data.HashMap.Lazy.fromList   (1 million)|    161,155,384    |314  |


<!-- RESULTS -->

## Insert Int (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|617.5 ns|11.83 μs|250.7 μs|4.298 ms|
|Data.Map.Strict|684.2 ns|13.15 μs|273.7 μs|4.653 ms|
|Data.HashMap.Lazy|507.1 ns|6.561 μs|106.2 μs|3.726 ms|
|Data.HashMap.Strict|503.1 ns|6.591 μs|104.7 μs|3.766 ms|
|Data.IntMap.Lazy|240.0 ns|3.830 μs|57.70 μs|1.856 ms|
|Data.IntMap.Strict|305.7 ns|4.678 μs|67.54 μs|1.997 ms|

## IO Insert Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|0.823 μs|19.54 μs|197.0 μs|2.411 ms|30.70 ms|360.1 ms|
|Data.HashTable.IO.LinearHashTable|0.999 μs|21.31 μs|211.9 μs|3.440 ms|63.49 ms|1416 ms|
|Data.HashTable.IO.CuckooHashTable|2.829 μs|44.97 μs|685.7 μs|9.773 ms|146.7 ms|1649 ms|
|Data.Judy|1.426 μs|12.40 μs|129.1 μs|1.153 ms|11.63 ms|124.6 ms|

## Intersection (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|1239 ns|13.83 μs|159.7 μs|1681 μs|20.23 ms|203.1 ms|
|Data.Map.Strict|1223 ns|13.87 μs|158.7 μs|1666 μs|20.22 ms|202.1 ms|
|Data.HashMap.Lazy|273.0 ns|2.500 μs|32.97 μs|379.7 μs|5.253 ms|53.77 ms|
|Data.HashMap.Strict|277.0 ns|2.493 μs|33.63 μs|380.8 μs|5.305 ms|53.61 ms|
|Data.IntMap.Lazy|112.8 ns|0.631 μs|7.388 μs|153.6 μs|3.632 ms|34.23 ms|
|Data.IntMap.Strict|113.0 ns|0.626 μs|7.355 μs|154.8 μs|3.627 ms|33.05 ms|

## Lookup Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|22.31 ns|29.50 ns|38.24 ns|51.16 ns|62.39 ns|63.60 ns|
|Data.Map.Strict|22.43 ns|30.26 ns|37.60 ns|52.53 ns|62.33 ns|63.48 ns|
|Data.HashMap.Lazy|49.79 ns|52.38 ns|28.11 ns|30.23 ns|58.84 ns|77.18 ns|
|Data.HashMap.Strict|49.73 ns|52.54 ns|28.11 ns|30.05 ns|59.23 ns|76.42 ns|
|Data.IntMap.Lazy|20.64 ns|28.83 ns|34.70 ns|39.34 ns|47.17 ns|62.30 ns|
|Data.IntMap.Strict|20.62 ns|28.76 ns|34.63 ns|39.31 ns|46.63 ns|67.19 ns|

## IO Lookup Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|36.81 ns|43.21 ns|37.22 ns|40.08 ns|36.60 ns|37.16 ns|
|Data.HashTable.IO.LinearHashTable|78.68 ns|86.21 ns|86.94 ns|79.55 ns|82.85 ns|292.8 ns|
|Data.HashTable.IO.CuckooHashTable|112.5 ns|111.5 ns|113.2 ns|111.7 ns|112.0 ns|110.8 ns|
|Data.Judy|55.59 ns|76.84 ns|66.85 ns|86.96 ns|57.18 ns|87.61 ns|

## FromList ByteString (Monotonic)

|Name|10000|
|---|---|
|Data.Map.Lazy|6.196 ms|
|Data.Map.Strict|6.474 ms|
|Data.HashMap.Lazy|3.208 ms|
|Data.HashMap.Strict|3.268 ms|
|Data.Trie|15.05 ms|

## FromList ByteString (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|869.3 ns|17.89 μs|366.0 μs|8.921 ms|
|Data.Map.Strict|913.3 ns|19.26 μs|383.9 μs|9.060 ms|
|Data.HashMap.Lazy|823.6 ns|10.76 μs|150.2 μs|3.922 ms|
|Data.HashMap.Strict|852.9 ns|10.98 μs|152.6 μs|3.913 ms|
|Data.Trie|1190 ns|25.30 μs|1359 μs|23.75 ms|

