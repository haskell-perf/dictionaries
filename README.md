# dictionaries

Benchmarks for dictionary data structures: hash tables, maps, tries,
etc.

The `judy` package was removed from this test suite for instability;
it segfaults the program.

## Running

For all benchmarks:

    $ stack bench :space

For just space:

    $ stack bench :space

For just time:

    $ stack bench :time

## Insert Int keys space use

|Case|                                              Bytes|    GCs|
|---|---|---|
|Data.Map.Strict.insert mempty                        |64      |0  |
|Data.Map.Lazy.insert mempty                          |64      |0  |
|Data.HashMap.Strict.insert mempty                    |64      |0  |
|Data.HashMap.Lazy.insert mempty                      |48      |0  |

## Pure maps fromList space use

| Case                                     | Total bytes   | Max residency | Final live | GCs   |
|------------------------------------------|---------------|---------------|------------|-------|
| Data.Map.Strict.fromList     (1 million) | 1,016,187,152 | 55,394,296    | 31,864     | 1,942 |
| Data.Map.Lazy.fromList       (1 million) | 1,016,187,152 | 55,394,296    | 31,864     | 1,942 |
| Data.IntMap.Strict.fromList  (1 million) | 776,852,648   | 55,207,424    | 31,864     | 1,489 |
| Data.IntMap.Lazy.fromList    (1 million) | 776,852,648   | 55,207,424    | 31,864     | 1,489 |
| Data.HashMap.Strict.fromList (1 million) | 161,155,384   | 40,358,064    | 0          | 314   |
| Data.HashMap.Lazy.fromList   (1 million) | 161,155,384   | 40,358,064    | 0          | 314   |

## IO maps fromList space use

| Case                                          | Total bytes | Max residency | Final live | GCs |
|-----------------------------------------------|-------------|---------------|------------|-----|
| Data.HashTable.IO.BasicHashTable (1 million)  | 424,214,184 | 47,254,400    | 1,120      | 672 |
| Data.HashTable.IO.CuckooHashTable (1 million) | 173,581,848 | 1,328         | 1,328      | 244 |
| Data.HashTable.IO.LinearHashTable (1 million) | 281,294,784 | 22,373,256    | 0          | 545 |
| Data.Judy                                     | 112,000,400 | 152           | 152        | 214 |

<!-- RESULTS -->

## Insert Int (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|532.6 ns|10.95 μs|246.3 μs|3.888 ms|
|Data.Map.Strict|613.7 ns|12.86 μs|251.6 μs|4.337 ms|
|Data.HashMap.Lazy|448.1 ns|6.475 μs|112.9 μs|3.681 ms|
|Data.HashMap.Strict|482.3 ns|6.147 μs|107.7 μs|3.578 ms|
|Data.IntMap.Lazy|207.8 ns|3.459 μs|50.05 μs|1.695 ms|
|Data.IntMap.Strict|239.8 ns|3.955 μs|62.72 μs|1.824 ms|

## IO Insert Int (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|1501 ns|12.87 μs|132.3 μs|2.628 ms|
|Data.HashTable.IO.LinearHashTable|914.0 ns|9.915 μs|111.9 μs|1.024 ms|
|Data.HashTable.IO.CuckooHashTable|1027 ns|10.35 μs|104.2 μs|1.562 ms|
|Data.Judy|440.8 ns|5.487 μs|68.25 μs|0.612 ms|

## Intersection (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|1049 ns|12.60 μs|156.2 μs|1578 μs|21.66 ms|197.8 ms|
|Data.Map.Strict|1042 ns|13.67 μs|151.5 μs|1602 μs|19.97 ms|203.0 ms|
|Data.HashMap.Lazy|177.2 ns|2.213 μs|30.00 μs|357.9 μs|5.128 ms|51.95 ms|
|Data.HashMap.Strict|179.3 ns|2.471 μs|30.19 μs|358.4 μs|5.284 ms|51.65 ms|
|Data.IntMap.Lazy|94.40 ns|0.620 μs|6.229 μs|163.0 μs|4.011 ms|33.22 ms|
|Data.IntMap.Strict|95.13 ns|0.617 μs|6.444 μs|164.8 μs|3.684 ms|32.42 ms|

## IO Intersection (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|2.470 μs|44.41 μs|648.0 μs|8.616 ms|
|Data.HashTable.IO.LinearHashTable|3.064 μs|40.86 μs|471.9 μs|8.734 ms|
|Data.HashTable.IO.CuckooHashTable|3.598 μs|48.18 μs|1206 μs|14.65 ms|
|Data.Judy|3.150 μs|41.50 μs|436.7 μs|6.138 ms|

## Lookup Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|17.11 ns|26.66 ns|35.41 ns|56.09 ns|75.62 ns|64.22 ns|
|Data.Map.Strict|17.51 ns|26.25 ns|35.62 ns|51.95 ns|71.76 ns|72.39 ns|
|Data.HashMap.Lazy|26.63 ns|29.82 ns|24.89 ns|28.10 ns|40.25 ns|44.14 ns|
|Data.HashMap.Strict|27.19 ns|31.86 ns|25.14 ns|32.25 ns|40.18 ns|48.39 ns|
|Data.IntMap.Lazy|17.70 ns|27.10 ns|35.27 ns|42.06 ns|49.01 ns|66.90 ns|
|Data.IntMap.Strict|17.91 ns|26.49 ns|33.68 ns|41.21 ns|47.23 ns|67.03 ns|

## IO Lookup Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|39.09 ns|45.62 ns|39.40 ns|35.00 ns|38.11 ns|36.18 ns|
|Data.HashTable.IO.LinearHashTable|76.05 ns|77.62 ns|90.57 ns|70.39 ns|77.03 ns|311.3 ns|
|Data.HashTable.IO.CuckooHashTable|78.33 ns|77.49 ns|87.11 ns|84.02 ns|77.63 ns|78.82 ns|
|Data.Judy|49.31 ns|70.85 ns|63.10 ns|88.21 ns|54.02 ns|82.28 ns|

## FromList ByteString (Monotonic)

|Name|10000|
|---|---|
|Data.Map.Lazy|5.672 ms|
|Data.Map.Strict|6.056 ms|
|Data.HashMap.Lazy|3.099 ms|
|Data.HashMap.Strict|3.357 ms|
|Data.Trie|15.65 ms|

## FromList ByteString (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|738.9 ns|17.80 μs|395.1 μs|11.27 ms|
|Data.Map.Strict|846.4 ns|19.68 μs|434.6 μs|10.94 ms|
|Data.HashMap.Lazy|680.0 ns|9.739 μs|148.0 μs|4.352 ms|
|Data.HashMap.Strict|709.0 ns|9.864 μs|150.8 μs|4.707 ms|
|Data.Trie|1062 ns|24.88 μs|1427 μs|25.57 ms|

## LookupByteString Monotonic

|Name|10000|
|---|---|
|Data.Map.Lazy|196.2 ns|
|Data.Map.Strict|198.6 ns|
|Data.HashMap.Lazy|45.37 ns|
|Data.HashMap.Strict|45.69 ns|
|Data.Trie|220.2 ns|

## LookupByteString Randomized

|Name|10000|
|---|---|
|Data.Map.Lazy|218.7 ns|
|Data.Map.Strict|217.0 ns|
|Data.HashMap.Lazy|59.10 ns|
|Data.HashMap.Strict|59.01 ns|
|Data.Trie|273.3 ns|
