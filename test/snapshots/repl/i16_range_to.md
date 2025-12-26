# META
~~~ini
description=I16.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1i16.to(5i16)
» 0i16.to(0i16)
» 5i16.to(3i16)
» -3i16.to(2i16)
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
