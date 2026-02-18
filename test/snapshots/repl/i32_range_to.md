# META
~~~ini
description=I32.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» -2.I32.to(2.I32)
» 0.I32.to(0.I32)
» 3.I32.to(1.I32)
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
