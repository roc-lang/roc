# META
~~~ini
description=I128.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.I128..<2.I128, [], |acc, item| acc.append(item))
» Iter.fold(1.I128..<1.I128, [], |acc, item| acc.append(item))
» Iter.fold(3.I128..<1.I128, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1]
---
[]
---
[]
# PROBLEMS
NIL
