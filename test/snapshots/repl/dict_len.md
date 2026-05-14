# META
~~~ini
description=Dict.len returns the number of key-value pairs
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().len()
» Dict.single("k", 1).len()
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).len()
~~~
# OUTPUT
0
---
1
---
3
# PROBLEMS
NIL
