# META
~~~ini
description=Dict.update modifies an existing value when the alter function returns Ok(new_value)
type=repl
~~~
# SOURCE
~~~roc
» Dict.update(Dict.single("k", 10), "k", |_| Ok(99)).get("k")
~~~
# OUTPUT
Ok(99.0)
# PROBLEMS
NIL
