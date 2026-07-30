# META
~~~ini
description=F32.range_exclusive - creates an iterator of floats from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.5.F32..<3.5.F32, [], |acc, item| acc.append(item))
» Iter.fold(2.0.F32..<2.0.F32, [], |acc, item| acc.append(item))
» Iter.fold(3.5.F32..<2.5.F32, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0.5, 1.5, 2.5]
---
[]
---
[]
# PROBLEMS
NIL
