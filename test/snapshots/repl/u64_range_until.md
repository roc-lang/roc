# META
~~~ini
description=U64.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.U64..<3.U64, [], |acc, item| acc.append(item))
» Iter.fold(1.U64..<1.U64, [], |acc, item| acc.append(item))
» Iter.fold(5.U64..<3.U64, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
# PROBLEMS
NIL
