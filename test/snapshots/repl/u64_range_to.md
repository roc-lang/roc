# META
~~~ini
description=U64.to - creates a list of integers from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» 1u64.to(5u64)
» 0u64.to(0u64)
» 5u64.to(3u64)
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL

