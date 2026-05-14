# META
~~~ini
description=Dict.update inserts a new key when the alter function returns Ok(value) for a missing key
type=repl
~~~
# SOURCE
~~~roc
» Dict.update(Dict.empty(), "a", |_| Ok(42)).get("a")
~~~
# OUTPUT
Ok(42.0)
# PROBLEMS
NIL
