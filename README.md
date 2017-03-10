# Dictionaries

In computer science, an associative array, map, symbol table, or dictionary is an abstract data type composed of a collection of (key, value) pairs, such that each possible key appears at most once in the collection.

From [Wikipedia](https://en.wikipedia.org/wiki/Associative_array).

## Running

For all benchmarks:

    $ stack bench :space

For just space:

    $ stack bench :space

For just time:

    $ stack bench :time

## Insert Int

```
Case                                              Bytes    GCs  Check
Data.Map.Strict.insert mempty                        64      0  OK
Data.Map.Lazy.insert mempty                          64      0  OK
Data.HashMap.Strict.insert mempty                    64      0  OK
Data.HashMap.Lazy.insert mempty                      48      0  OK
```

```
benchmarking Consing/Data.Map.Lazy 0..10
time                 591.1 ns   (587.1 ns .. 595.4 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 592.4 ns   (588.8 ns .. 597.4 ns)
std dev              14.50 ns   (11.25 ns .. 17.80 ns)
variance introduced by outliers: 33% (moderately inflated)

benchmarking Consing/Data.Map.Strict 0..10
time                 657.2 ns   (652.3 ns .. 663.1 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 660.6 ns   (655.8 ns .. 666.1 ns)
std dev              17.58 ns   (14.79 ns .. 22.62 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Consing/Data.HashMap.Lazy 0..10
time                 494.1 ns   (490.0 ns .. 499.1 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 496.7 ns   (491.9 ns .. 504.7 ns)
std dev              20.59 ns   (14.47 ns .. 33.23 ns)
variance introduced by outliers: 59% (severely inflated)

benchmarking Consing/Data.HashMap.Strict 0..10
time                 506.7 ns   (502.1 ns .. 512.3 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 508.3 ns   (503.8 ns .. 514.3 ns)
std dev              17.11 ns   (13.21 ns .. 24.94 ns)
variance introduced by outliers: 48% (moderately inflated)

benchmarking Consing/Data.IntMap.Lazy 0..10
time                 238.0 ns   (236.1 ns .. 239.8 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 236.8 ns   (235.3 ns .. 238.6 ns)
std dev              5.790 ns   (4.880 ns .. 6.899 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Consing/Data.IntMap.Strict 0..10
time                 308.8 ns   (305.8 ns .. 311.8 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 310.3 ns   (307.1 ns .. 314.1 ns)
std dev              11.70 ns   (9.412 ns .. 15.33 ns)
variance introduced by outliers: 55% (severely inflated)

benchmarking Consing/Data.Map.Lazy 0..1000
time                 242.5 μs   (240.1 μs .. 244.8 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 243.2 μs   (241.5 μs .. 245.4 μs)
std dev              6.383 μs   (4.986 μs .. 8.818 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking Consing/Data.Map.Strict 0..1000
time                 265.2 μs   (262.9 μs .. 267.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 264.8 μs   (263.1 μs .. 267.4 μs)
std dev              7.092 μs   (4.892 μs .. 9.728 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking Consing/Data.HashMap.Lazy 0..1000
time                 103.9 μs   (102.9 μs .. 104.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 103.4 μs   (102.7 μs .. 104.4 μs)
std dev              2.938 μs   (2.214 μs .. 4.355 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking Consing/Data.HashMap.Strict 0..1000
time                 102.9 μs   (101.6 μs .. 104.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 102.8 μs   (102.1 μs .. 104.1 μs)
std dev              3.350 μs   (2.288 μs .. 5.204 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking Consing/Data.IntMap.Lazy 0..1000
time                 54.51 μs   (54.13 μs .. 54.91 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 54.73 μs   (54.26 μs .. 55.55 μs)
std dev              2.021 μs   (1.261 μs .. 2.908 μs)
variance introduced by outliers: 40% (moderately inflated)

benchmarking Consing/Data.IntMap.Strict 0..1000
time                 64.04 μs   (63.38 μs .. 64.74 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 63.61 μs   (63.27 μs .. 64.15 μs)
std dev              1.404 μs   (1.135 μs .. 1.746 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking Consing/Data.Map.Lazy 0..10000
time                 4.146 ms   (4.090 ms .. 4.208 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 4.211 ms   (4.169 ms .. 4.269 ms)
std dev              162.0 μs   (123.4 μs .. 210.4 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Consing/Data.Map.Strict 0..10000
time                 4.621 ms   (4.568 ms .. 4.682 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 4.580 ms   (4.542 ms .. 4.628 ms)
std dev              132.7 μs   (104.9 μs .. 190.4 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Consing/Data.HashMap.Lazy 0..10000
time                 3.675 ms   (3.635 ms .. 3.707 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 3.703 ms   (3.667 ms .. 3.773 ms)
std dev              154.8 μs   (84.93 μs .. 270.2 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking Consing/Data.HashMap.Strict 0..10000
time                 3.768 ms   (3.692 ms .. 3.867 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 3.925 ms   (3.869 ms .. 3.997 ms)
std dev              198.6 μs   (150.2 μs .. 271.4 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking Consing/Data.IntMap.Lazy 0..10000
time                 1.734 ms   (1.710 ms .. 1.760 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 1.845 ms   (1.807 ms .. 1.894 ms)
std dev              145.9 μs   (115.2 μs .. 185.0 μs)
variance introduced by outliers: 58% (severely inflated)

benchmarking Consing/Data.IntMap.Strict 0..10000
time                 1.885 ms   (1.864 ms .. 1.907 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.895 ms   (1.878 ms .. 1.934 ms)
std dev              86.84 μs   (40.09 μs .. 168.2 μs)
variance introduced by outliers: 31% (moderately inflated)
```

## From Int List

```
Case                                              Bytes    GCs  Check
Data.Map.Strict.fromList     (1 million)  1,016,187,152  1,942  OK
Data.Map.Lazy.fromList       (1 million)  1,016,187,152  1,942  OK
Data.IntMap.Strict.fromList  (1 million)    776,852,648  1,489  OK
Data.IntMap.Lazy.fromList    (1 million)    776,852,648  1,489  OK
Data.HashMap.Strict.fromList (1 million)    161,155,384    314  OK
Data.HashMap.Lazy.fromList   (1 million)    161,155,384    314  OK
```
