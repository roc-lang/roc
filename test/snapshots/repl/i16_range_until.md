# META
~~~ini
description=I16.until - creates a list of integers from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» 0i16.until(3i16)
» 1i16.until(1i16)
» 5i16.until(3i16)
» -2i16.until(2i16)
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
