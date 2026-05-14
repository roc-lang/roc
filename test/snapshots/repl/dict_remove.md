# META
~~~ini
description=Dict.remove deletes the value at a given key
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).remove("a").len()
» Dict.empty().insert("a", 1).insert("b", 2).remove("a").contains("a")
» Dict.empty().insert("a", 1).remove("missing").len()
~~~
# OUTPUT
1
---
False
---
1
# PROBLEMS
NIL
