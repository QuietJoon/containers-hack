Plan
====

# Optimize `Data.IntMap.Bound`

In current status, the function have heavy and not optimized feature.
This just finished debugging, and is not desinged for handling negative numbers.

# Implement `deleteBounded`

Can implement `deleteBounded` by using `fold` and `bounded`.
But could be more efficient with internal functions.
