# Sequences

In computer science, a list or sequence is an abstract data type
that represents a countable number of ordered values, where the same
value may occur more than once. An instance of a list is a computer
representation of the mathematical concept of a finite sequence.

From [Wikipedia](https://en.wikipedia.org/wiki/List_(abstract_data_type)).

## Running

For all benchmarks:

    $ stack bench :space

For specific benchmarks:

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
