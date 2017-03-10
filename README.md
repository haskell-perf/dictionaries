# Dictionaries

In computer science, an associative array, map, symbol table, or dictionary is an abstract data type composed of a collection of (key, value) pairs, such that each possible key appears at most once in the collection.

From [Wikipedia](https://en.wikipedia.org/wiki/Associative_array).

## Running

For all benchmarks:

    $ stack bench :space

## Insert

```
Case                                              Bytes    GCs  Check
Data.Map.Strict.insert mempty                        64      0  OK
Data.Map.Lazy.insert mempty                          64      0  OK
Data.HashMap.Strict.insert mempty                    64      0  OK
Data.HashMap.Lazy.insert mempty                      48      0  OK
```

## From List

```
Case                                              Bytes    GCs  Check
Data.Map.Strict.fromList     (1 million)  1,016,187,152  1,942  OK
Data.Map.Lazy.fromList       (1 million)  1,016,187,152  1,942  OK
Data.IntMap.Strict.fromList  (1 million)    776,852,648  1,489  OK
Data.IntMap.Lazy.fromList    (1 million)    776,852,648  1,489  OK
Data.HashMap.Strict.fromList (1 million)    161,155,384    314  OK
Data.HashMap.Lazy.fromList   (1 million)    161,155,384    314  OK
```
