# META
~~~ini
description=Dict.keep_shared keeps pairs that match in both dictionaries
type=repl
~~~
# SOURCE
~~~roc
» Dict.keep_shared(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("a", 1).insert("b", 99)).len()
» Dict.keep_shared(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("a", 1).insert("b", 99)).get("a")
» Dict.keep_shared(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("a", 1).insert("b", 99)).contains("b")
~~~
# OUTPUT
1
---
Ok(1.0)
---
False
# PROBLEMS
NIL
