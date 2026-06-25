# META
~~~ini
description=I64.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.I64..<2.I64, [], |acc, item| acc.append(item))
» Iter.fold(1.I64..<1.I64, [], |acc, item| acc.append(item))
» Iter.fold(3.I64..<1.I64, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1]
---
[]
---
[]
# PROBLEMS
NIL
