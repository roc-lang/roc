# META
~~~ini
description=List.clear removes every element while preserving the current capacity
type=repl
~~~
# SOURCE
~~~roc
» [1.I64, 2, 3, 4].clear()
» original = ["keep this long heap-allocated string that will not fit inline", "and this other long heap-allocated string too"]
» original.clear()
» original
~~~
# OUTPUT
[]
---
assigned `original`
---
[]
---
["keep this long heap-allocated string that will not fit inline", "and this other long heap-allocated string too"]
# PROBLEMS
NIL
