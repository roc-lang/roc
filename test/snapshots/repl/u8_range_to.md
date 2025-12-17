# META
~~~ini
description=U8.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1u8.to(5u8)
» 0u8.to(0u8)
» 5u8.to(3u8)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
