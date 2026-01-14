# META
~~~ini
description=U64.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1.U64.to(5.U64)
» 0.U64.to(0.U64)
» 5.U64.to(3.U64)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
