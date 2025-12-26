# META
~~~ini
description=U128.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1u128.to(5u128)
» 0u128.to(0u128)
» 5u128.to(3u128)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL

