# META
~~~ini
description=I64.until - creates a list of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» 0i64.until(3i64)
» 1i64.until(1i64)
» 5i64.until(3i64)
» -2i64.until(2i64)
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
