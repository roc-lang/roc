# META
~~~ini
description=Str.release_excess_capacity should return the same string with excess capacity released
type=repl
~~~
# SOURCE
~~~roc
» Str.release_excess_capacity("hello")
» Str.release_excess_capacity("")
» Str.release_excess_capacity("hello world")
~~~
# OUTPUT
"hello"
---
""
---
"hello world"
# PROBLEMS
NIL
