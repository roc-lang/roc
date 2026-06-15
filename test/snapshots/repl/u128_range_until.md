# META
~~~ini
description=U128.until - creates an iterator of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.U128..<3.U128, [], |acc, item| acc.append(item))
» Iter.fold(1.U128..<1.U128, [], |acc, item| acc.append(item))
» Iter.fold(5.U128..<3.U128, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
# PROBLEMS
NIL
