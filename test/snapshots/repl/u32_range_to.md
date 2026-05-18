# META
~~~ini
description=U32.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1.U32.to(5.U32)
» 0.U32.to(0.U32)
» 5.U32.to(3.U32)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
