# META
~~~ini
description=U8.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1.U8.to(5.U8)
» 0.U8.to(0.U8)
» 5.U8.to(3.U8)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
