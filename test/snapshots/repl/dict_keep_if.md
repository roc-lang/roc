# META
~~~ini
description=Dict.keep_if keeps the pairs for which the predicate returns True
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).keep_if(|(_k, v)| v >= 2).len()
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).keep_if(|(_k, v)| v >= 2).get("a")
~~~
# OUTPUT
2
---
Err(KeyNotFound)
# PROBLEMS
NIL
