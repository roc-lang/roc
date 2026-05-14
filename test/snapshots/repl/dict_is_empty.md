# META
~~~ini
description=Dict.is_empty reports whether the dictionary has no entries
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().is_empty()
» Dict.single("k", 1).is_empty()
» Dict.empty().insert("a", 1).remove("a").is_empty()
~~~
# OUTPUT
True
---
False
---
True
# PROBLEMS
NIL
