# META
~~~ini
description=U8.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.U8..<3.U8, [], |acc, item| acc.append(item))
» Iter.fold(1.U8..<1.U8, [], |acc, item| acc.append(item))
» Iter.fold(5.U8..<3.U8, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
# PROBLEMS
NIL
