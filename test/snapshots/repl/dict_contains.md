# META
~~~ini
description=Dict.contains reports whether a key is present
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).contains("a")
» Dict.empty().insert("a", 1).contains("missing")
~~~
# OUTPUT
True
---
False
# PROBLEMS
NIL
