# META
~~~ini
description=Str.join_with should join strings with a separator
type=repl
~~~
# SOURCE
~~~roc
» Str.join_with(["hello", "world"], " ")
» Str.join_with(["a", "b", "c"], ",")
» Str.join_with(["single"], "-")
» Str.join_with([], ",")
~~~
# OUTPUT
"hello world"
---
"a,b,c"
---
"single"
---
""
# PROBLEMS
NIL
