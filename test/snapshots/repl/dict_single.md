# META
~~~ini
description=Dict.single creates a dictionary with one key-value pair
type=repl
~~~
# SOURCE
~~~roc
» Dict.single("k", 1).len()
» Dict.single("k", 1).get("k")
~~~
# OUTPUT
1
---
Ok(1.0)
# PROBLEMS
NIL
