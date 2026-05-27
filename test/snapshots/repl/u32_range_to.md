# META
~~~ini
description=U32.to - creates an iterator of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(1.U32.to(5.U32), [], |acc, item| acc.append(item))
» Iter.fold(0.U32.to(0.U32), [], |acc, item| acc.append(item))
» Iter.fold(5.U32.to(3.U32), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
