# META
~~~ini
description=List.join_with should dispatch to Str.join_with for List(Str)
type=repl
~~~
# SOURCE
~~~roc
» List.join_with(["hello", "world"], " ")
» List.join_with(["a", "b", "c"], ",")
» List.join_with(["single"], "-")
» List.join_with([], ",")
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
