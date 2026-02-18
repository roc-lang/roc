# META
~~~ini
description=I128.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» -2.I128.to(2.I128)
» 0.I128.to(0.I128)
» 3.I128.to(1.I128)
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
