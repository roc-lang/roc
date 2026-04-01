# META
~~~ini
description=Str.with_capacity should create an empty string with preallocated capacity
type=repl
~~~
# SOURCE
~~~roc
» Str.with_capacity(0)
» Str.with_capacity(10)
» Str.with_capacity(100)
~~~
# OUTPUT
""
---
""
---
""
# PROBLEMS
NIL
