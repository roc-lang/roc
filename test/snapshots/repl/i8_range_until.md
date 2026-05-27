# META
~~~ini
description=I8.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.I8.until(2.I8), [], |acc, item| acc.append(item))
» Iter.fold(1.I8.until(1.I8), [], |acc, item| acc.append(item))
» Iter.fold(3.I8.until(1.I8), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1]
---
[]
---
[]
# PROBLEMS
NIL
