# META
~~~ini
description=I128.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(-2.I128..=2.I128, [], |acc, item| acc.append(item))
» Iter.fold(0.I128..=0.I128, [], |acc, item| acc.append(item))
» Iter.fold(3.I128..=1.I128, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
