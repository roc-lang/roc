# META
~~~ini
description=U32.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1u32.to(5u32)
» 0u32.to(0u32)
» 5u32.to(3u32)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
