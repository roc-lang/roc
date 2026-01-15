# META
~~~ini
description=U128.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1.U128.to(5.U128)
» 0.U128.to(0.U128)
» 5.U128.to(3.U128)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
