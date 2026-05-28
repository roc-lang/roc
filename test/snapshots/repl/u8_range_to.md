# META
~~~ini
description=U8.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(1.U8.to(5.U8), [], |acc, item| acc.append(item))
» Iter.fold(0.U8.to(0.U8), [], |acc, item| acc.append(item))
» Iter.fold(5.U8.to(3.U8), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
