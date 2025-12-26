# META
~~~ini
description=I8.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1i8.to(5i8)
» 0i8.to(0i8)
» 5i8.to(3i8)
» -3i8.to(2i8)
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
