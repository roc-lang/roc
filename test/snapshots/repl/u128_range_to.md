# META
~~~ini
description=U128.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(1.U128..=5.U128, [], |acc, item| acc.append(item))
» Iter.fold(0.U128..=0.U128, [], |acc, item| acc.append(item))
» Iter.fold(5.U128..=3.U128, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
