# META
~~~ini
description=Dict.from_list creates a dictionary from a list of key-value tuples
type=repl
~~~
# SOURCE
~~~roc
» Dict.from_list([("a", 1), ("b", 2)]).get("a")
» Dict.from_list([("a", 1), ("b", 2)]).get("b")
» Dict.from_list([("a", 1), ("b", 2)]).len()
~~~
# OUTPUT
Ok(1.0)
---
Ok(2.0)
---
2
# PROBLEMS
NIL
