# META
~~~ini
description=Dict.insert_all unions two dictionaries, second wins on key collision
type=repl
~~~
# SOURCE
~~~roc
» Dict.insert_all(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("b", 99).insert("c", 3)).len()
» Dict.insert_all(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("b", 99).insert("c", 3)).get("b")
» Dict.insert_all(Dict.empty().insert("a", 1).insert("b", 2), Dict.empty().insert("b", 99).insert("c", 3)).get("c")
~~~
# OUTPUT
3
---
Ok(99.0)
---
Ok(3.0)
# PROBLEMS
NIL
