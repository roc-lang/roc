# META
~~~ini
description=Dict.iter yields the key-value pairs in the same order as Dict.to_list
type=repl
~~~
# SOURCE
~~~roc
» d = Dict.empty().insert("alice", "a heap allocated value").insert("bob", "another heap value")
» List.from_iter(d.iter())
» List.from_iter(Dict.empty().iter())
» d.len()
~~~
# OUTPUT
assigned `d`
---
[("alice", "a heap allocated value"), ("bob", "another heap value")]
---
[]
---
2
# PROBLEMS
NIL
