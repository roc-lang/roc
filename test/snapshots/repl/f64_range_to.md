# META
~~~ini
description=F64.range_inclusive - creates an iterator of floats from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.0.F64..=2.0.F64, [], |acc, item| acc.append(item))
» Iter.fold(0.0.F64..=0.0.F64, [], |acc, item| acc.append(item))
» Iter.fold(3.0.F64..=1.0.F64, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
