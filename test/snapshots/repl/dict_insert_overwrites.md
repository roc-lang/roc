# META
~~~ini
description=Dict.insert replaces an existing value at the same key
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("a", 99).get("a")
» Dict.empty().insert("a", 1).insert("a", 99).len()
~~~
# OUTPUT
Ok(99.0)
---
1
# PROBLEMS
NIL
