# META
~~~ini
description=Dict.from_iter builds a dictionary from an iterator of key-value pairs
type=repl
~~~
# SOURCE
~~~roc
» l = [("alice", "a heap allocated value"), ("bob", "another heap value")]
» d = Dict.from_iter(l.iter())
» d.get("alice")
» d.get("missing")
» l
» d.len()
» Dict.from_iter([("alice", "first"), ("alice", "second")].iter()).get("alice")
» Dict.from_iter([("alice", 1.I64), ("bob", 2)].iter()).to_list()
» Dict.from_iter([("alice", 1.I64), ("bob", 2)].iter().keep_if(|(_k, v)| v == 2)).to_list()
~~~
# OUTPUT
assigned `l`
---
assigned `d`
---
Ok("a heap allocated value")
---
Err(KeyNotFound)
---
[("alice", "a heap allocated value"), ("bob", "another heap value")]
---
2
---
Ok("second")
---
[("alice", 1), ("bob", 2)]
---
[("bob", 2)]
# PROBLEMS
NIL
