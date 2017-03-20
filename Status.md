Status
====

# 0.0.0.1

## Function

Works for only positive number arguments only.

## Performance

Refer `bench/B.html`.

Compare to `IntMap` filter and `List` filter, the present function reduce runtime clearly.

About ~100 size of target, the present function is 25% faster than other implementation.
About ~1000 and ~10000 size of target, the present function is 15% faster than other implementation.

The function is planned to target 10~100 sized queue. Therefore, the result shows that the function satisfies its purpose enough.
