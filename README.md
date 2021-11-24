# dictionaries

Benchmarks for dictionary data structures: hash tables, maps, tries,
etc.

The `judy` package was removed from this test suite for instability;
it segfaults the program.

## Running

For all benchmarks:

    $ stack bench

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

<!-- RESULTS -->

## Insert Int (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|406.3 ns|7.695 μs|137.3 μs|2.349 ms|
|Data.Map.Strict|473.1 ns|9.485 μs|165.3 μs|2.769 ms|
|Data.HashMap.Lazy|287.2 ns|3.949 μs|53.47 μs|1.741 ms|
|Data.HashMap.Strict|291.8 ns|3.948 μs|53.25 μs|1.711 ms|
|Data.IntMap.Lazy|136.4 ns|2.119 μs|30.13 μs|0.878 ms|
|Data.IntMap.Strict|161.3 ns|2.878 μs|39.46 μs|0.985 ms|

## IO Insert Int (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|356.1 ns|3.308 μs|33.70 μs|466.2 μs|
|Data.HashTable.IO.LinearHashTable|684.2 ns|7.321 μs|76.05 μs|660.1 μs|
|Data.HashTable.IO.CuckooHashTable|875.3 ns|8.493 μs|85.69 μs|943.4 μs|

## Intersection (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|422.4 ns|5.036 μs|63.85 μs|742.4 μs|10.38 ms|154.1 ms|
|Data.Map.Strict|437.6 ns|5.028 μs|65.15 μs|742.6 μs|9.070 ms|97.95 ms|
|Data.HashMap.Lazy|118.3 ns|1.332 μs|17.81 μs|225.2 μs|2.760 ms|37.95 ms|
|Data.HashMap.Strict|114.4 ns|1.315 μs|17.94 μs|225.5 μs|2.884 ms|38.64 ms|
|Data.IntMap.Lazy|66.73 ns|0.454 μs|5.146 μs|121.1 μs|1.533 ms|23.48 ms|
|Data.IntMap.Strict|66.86 ns|0.456 μs|5.115 μs|120.2 μs|1.524 ms|24.30 ms|

## IO Intersection (Randomized)

|Name|10|100|1000|10000|100000|
|---|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|212.9 ns|1.286 μs|17.44 μs|368.9 μs|9.504 ms|
|Data.HashTable.IO.LinearHashTable|262.8 ns|2.503 μs|25.17 μs|309.6 μs|14.84 ms|
|Data.HashTable.IO.CuckooHashTable|1010 ns|8.765 μs|84.19 μs|901.9 μs|19.21 ms|

## Intersection ByteString (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|947.2 ns|11.61 μs|197.5 μs|3.059 ms|46.70 ms|607.7 ms|
|Data.Map.Strict|1152 ns|15.69 μs|209.6 μs|3.149 ms|41.23 ms|500.9 ms|
|Data.HashMap.Lazy|533.9 ns|6.898 μs|81.07 μs|1.514 ms|24.18 ms|375.1 ms|
|Data.HashMap.Strict|648.7 ns|8.636 μs|80.14 μs|1.166 ms|24.13 ms|314.8 ms|
|Data.Trie|439.6 ns|6.474 μs|73.46 μs|1.755 ms|24.13 ms|245.4 ms|

## Lookup Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|97.96 ns|1.634 μs|52.42 μs|1023 μs|20.27 ms|697.9 ms|
|Data.Map.Strict|101.1 ns|1.632 μs|52.25 μs|970.9 μs|17.87 ms|583.0 ms|
|Data.HashMap.Lazy|133.6 ns|1.705 μs|22.95 μs|408.9 μs|8.338 ms|453.7 ms|
|Data.HashMap.Strict|132.9 ns|1.728 μs|22.42 μs|411.9 μs|8.540 ms|460.5 ms|
|Data.IntMap.Lazy|105.7 ns|1.756 μs|53.45 μs|895.8 μs|14.88 ms|710.6 ms|
|Data.IntMap.Strict|103.6 ns|1.688 μs|53.62 μs|883.4 μs|15.25 ms|700.8 ms|

## IO Lookup Int (Randomized)

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.HashTable.IO.BasicHashTable|15.24 ns|23.19 ns|14.62 ns|14.43 ns|14.34 ns|14.28 ns|
|Data.HashTable.IO.LinearHashTable|53.83 ns|58.41 ns|57.73 ns|53.36 ns|57.63 ns|145.0 ns|
|Data.HashTable.IO.CuckooHashTable|59.15 ns|57.50 ns|57.13 ns|58.15 ns|57.69 ns|56.47 ns|

## FromList ByteString (Monotonic)

|Name|10000|
|---|---|
|Data.Map.Lazy|3.584 ms|
|Data.Map.Strict|4.161 ms|
|Data.HashMap.Lazy|2.040 ms|
|Data.HashMap.Strict|2.075 ms|
|Data.Trie|8.717 ms|

## FromList ByteString (Randomized)

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|559.3 ns|11.60 μs|236.3 μs|4.496 ms|
|Data.Map.Strict|631.5 ns|13.54 μs|246.4 μs|5.082 ms|
|Data.HashMap.Lazy|547.8 ns|7.100 μs|96.10 μs|2.741 ms|
|Data.HashMap.Strict|557.0 ns|7.214 μs|98.62 μs|2.710 ms|
|Data.Trie|910.6 ns|15.71 μs|373.8 μs|14.93 ms|

## Lookup ByteString Monotonic

|Name|10000|
|---|---|
|Data.Map.Lazy|91.88 ns|
|Data.Map.Strict|91.35 ns|
|Data.HashMap.Lazy|27.23 ns|
|Data.HashMap.Strict|27.41 ns|
|Data.Trie|150.7 ns|

## Lookup ByteString Randomized

|Name|10000|
|---|---|
|Data.Map.Lazy|2.031 ms|
|Data.Map.Strict|1.915 ms|
|Data.HashMap.Lazy|0.678 ms|
|Data.HashMap.Strict|0.670 ms|
|Data.Trie|2.515 ms|

