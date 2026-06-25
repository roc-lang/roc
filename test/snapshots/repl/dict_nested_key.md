# META
~~~ini
description=Dict can be used as a dictionary key independent of insertion order
type=repl
~~~
# SOURCE
~~~roc
» key = Dict.empty().insert("a", 1).insert("b", 2)
» lookup = Dict.empty().insert("b", 2).insert("a", 1)
» Dict.empty().insert(key, "found").get(lookup)
~~~
# OUTPUT
assigned `key`
---
assigned `lookup`
---
Ok("found")
# PROBLEMS
NIL
