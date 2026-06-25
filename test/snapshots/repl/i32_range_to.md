# META
~~~ini
description=I32.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.I32..=2.I32, [], |acc, item| acc.append(item))
» Iter.fold(0.I32..=0.I32, [], |acc, item| acc.append(item))
» Iter.fold(3.I32..=1.I32, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
