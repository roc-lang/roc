# META
~~~ini
description=Dict.drop_if removes the pairs for which the predicate returns True
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).drop_if(|(_k, v)| v >= 2).len()
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).drop_if(|(_k, v)| v >= 2).get("a")
~~~
# OUTPUT
1
---
Ok(1.0)
# PROBLEMS
NIL
