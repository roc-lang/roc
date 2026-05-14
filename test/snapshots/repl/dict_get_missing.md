# META
~~~ini
description=Dict.get returns Err(KeyNotFound) for absent keys
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).get("missing")
» Dict.empty().get("missing")
~~~
# OUTPUT
Err(KeyNotFound)
---
Err(KeyNotFound)
# PROBLEMS
NIL
