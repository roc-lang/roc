# META
~~~ini
description=Dict.update removes the key when the alter function returns Err(Missing)
type=repl
~~~
# SOURCE
~~~roc
» Dict.update(Dict.single("k", 10), "k", |_| Err(Missing)).len()
~~~
# OUTPUT
0
# PROBLEMS
NIL
