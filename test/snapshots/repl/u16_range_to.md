# META
~~~ini
description=U16.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1.U16.to(5.U16)
» 0.U16.to(0.U16)
» 5.U16.to(3.U16)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
