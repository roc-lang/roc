# META
~~~ini
description=Dict.remove swap-removes entries and keeps remaining lookups valid
type=repl
~~~
# SOURCE
~~~roc
» dict = Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3)
» dict.remove("a").to_list()
» dict.remove("a").get("c")
» dict.remove("missing").to_list()
~~~
# OUTPUT
assigned `dict`
---
[("c", 3.0), ("b", 2.0)]
---
Ok(3.0)
---
[("a", 1.0), ("b", 2.0), ("c", 3.0)]
# PROBLEMS
NIL
