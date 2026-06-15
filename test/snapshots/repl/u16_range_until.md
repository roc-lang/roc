# META
~~~ini
description=U16.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.U16..<3.U16, [], |acc, item| acc.append(item))
» Iter.fold(1.U16..<1.U16, [], |acc, item| acc.append(item))
» Iter.fold(5.U16..<3.U16, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
# PROBLEMS
NIL
