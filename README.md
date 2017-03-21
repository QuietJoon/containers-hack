containers-hack
====

Some custom functions which is not present yet in `containers`, but needed for me.

# `Data.IntMap.Bound`

Functions to get a part of sequence in a `IntMap a`.

## Functions

### `limitedOf`

A function to get a part of sequence in a `IntMap a` by its key which are bigger or equal than lower bound and smaller or equal than upper bound.
It is equivalent to `filterWithKey (\k _ -> lowerBound <= k && k <= upperBound)`.

### `boundedOf`

A function to get a part of sequence in a `IntMap a` by its key which covers lower bound and upper bound.
It is equivalent to `limitedOf lowerBound upperBound t` + `lookupLT lowerBound t` + `lookupGT upperBound t`.

### `rough*`

Maybe internal function.
Bounding/Limiting at masking level.
This post process should be finished very quickly, and not duplicate with `boundedOf/limitedOf`.

## Proof & Test Status

### Proof

Not yet....
I nearly understand that how IntMap is constructed and how prefix and mask work, 
but I'm not sure that I understand it perfectly....

### Test

Tested by QuickCheck. This may not cover all of boundary conditions.
Tested cases are recorded in `testset.hs`.
