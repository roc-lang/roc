# META
~~~ini
description=Dict.insert adds key-value pairs, retrievable via Dict.get
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).get("a")
» Dict.empty().insert("a", 1).insert("b", 2).get("b")
~~~
# OUTPUT
Ok(1.0)
---
Ok(2.0)
# PROBLEMS
NIL
