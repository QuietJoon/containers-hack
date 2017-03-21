Issues
====

# Strict or Lazy

## `Data.IntMap.Bound`

For my initial purpose, `bounded` function should not be strict for accumulated-list.

The function is designed as like as traversing `IntMap` from the end to front.
This is just for the cost of constructing list. (how about `accumulated_list ++ [something]`?)

However, my purpose is that scanning from start to end.
This is changable by its strategy, but previous strategy is scanning it from front to end.

### Strict strategy

There could be a strategy that accumulated-list can be evaluated before evaluate the sub-function.
I tried making it strictly about accumulating-list of `goL` in `bounded`.

However, when the proram start to scanning it from front to end which can be finish the steps in the middle, strict strategy requires unnecessary cost.

Sometimes, the strictness may helps in some way.
However, the strictness is not always better.


