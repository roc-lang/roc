# META
~~~ini
description=I64.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» -2.I64.to(2.I64)
» 0.I64.to(0.I64)
» 3.I64.to(1.I64)
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
