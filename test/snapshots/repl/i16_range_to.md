# META
~~~ini
description=I16.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.I16..=2.I16, [], |acc, item| acc.append(item))
» Iter.fold(0.I16..=0.I16, [], |acc, item| acc.append(item))
» Iter.fold(3.I16..=1.I16, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
