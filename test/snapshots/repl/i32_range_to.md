# META
~~~ini
description=I32.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1i32.to(5i32)
» 0i32.to(0i32)
» 5i32.to(3i32)
» -3i32.to(2i32)
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
