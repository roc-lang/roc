# META
~~~ini
description=I128.until - creates a list of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» -2.I128.until(2.I128)
» 1.I128.until(1.I128)
» 3.I128.until(1.I128)
~~~
# OUTPUT
[-2, -1, 0, 1]
---
[]
---
[]
# PROBLEMS
NIL
