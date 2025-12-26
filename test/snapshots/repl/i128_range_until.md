# META
~~~ini
description=I128.until - creates a list of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» 0i128.until(3i128)
» 1i128.until(1i128)
» 5i128.until(3i128)
» -2i128.until(2i128)
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
---
[-2, -1, 0, 1]
# PROBLEMS
NIL
