# META
~~~ini
description=Dict.map transforms each value, leaving keys unchanged
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).map(|_k, v| v * 10).get("b")
» Dict.empty().insert("a", 1).insert("b", 2).map(|_k, v| v * 10).len()
~~~
# OUTPUT
Ok(20.0)
---
2
# PROBLEMS
NIL
