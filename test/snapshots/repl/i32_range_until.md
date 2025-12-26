# META
~~~ini
description=I32.until - creates a list of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» 0i32.until(3i32)
» 1i32.until(1i32)
» 5i32.until(3i32)
» -2i32.until(2i32)
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
