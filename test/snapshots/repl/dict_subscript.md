# META
~~~ini
description=Dict.subscript returns the value for a key, or Err(KeyNotFound) for absent keys
type=repl
~~~
# SOURCE
~~~roc
» d = Dict.empty().insert("alice", "a heap allocated value").insert("bob", "another heap value")
» d.subscript("alice")
» d.subscript("missing")
» d.subscript("bob")
» d.len()
~~~
# OUTPUT
assigned `d`
---
Ok("a heap allocated value")
---
Err(KeyNotFound)
---
Ok("another heap value")
---
2
# PROBLEMS
NIL
