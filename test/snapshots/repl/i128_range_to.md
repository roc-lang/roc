# META
~~~ini
description=I128.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1i128.to(5i128)
» 0i128.to(0i128)
» 5i128.to(3i128)
» -3i128.to(2i128)
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
