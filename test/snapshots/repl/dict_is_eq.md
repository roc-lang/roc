# META
~~~ini
description=Dict.is_eq compares dictionaries by content, ignoring insertion order
type=repl
~~~
# SOURCE
~~~roc
» Dict.is_eq(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("b", 2).insert("a", 1))
» Dict.is_eq(Dict.empty().insert("a", 1), Dict.empty().insert("a", 2))
» Dict.is_eq(Dict.empty().insert("a", 1), Dict.empty().insert("a", 1).insert("b", 2))
~~~
# OUTPUT
True
---
False
---
False
# PROBLEMS
NIL
