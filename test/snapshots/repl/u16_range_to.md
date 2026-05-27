# META
~~~ini
description=U16.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(1.U16.to(5.U16), [], |acc, item| acc.append(item))
» Iter.fold(0.U16.to(0.U16), [], |acc, item| acc.append(item))
» Iter.fold(5.U16.to(3.U16), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
