# META
~~~ini
description=I64.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1i64.to(5i64)
» 0i64.to(0i64)
» 5i64.to(3i64)
» -3i64.to(2i64)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
---
[-3, -2, -1, 0, 1, 2]
# PROBLEMS
NIL
