# META
~~~ini
description=Dict.remove_all drops any key from the first dict that's present in the second
type=repl
~~~
# SOURCE
~~~roc
» Dict.remove_all(Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3), Dict.empty().insert("b", 99).insert("d", 4)).len()
» Dict.remove_all(Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3), Dict.empty().insert("b", 99).insert("d", 4)).contains("b")
» Dict.remove_all(Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3), Dict.empty().insert("b", 99).insert("d", 4)).get("c")
~~~
# OUTPUT
2
---
False
---
Ok(3.0)
# PROBLEMS
NIL
