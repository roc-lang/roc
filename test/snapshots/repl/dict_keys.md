# META
~~~ini
description=Dict.keys returns the keys as a list
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).keys()
~~~
# OUTPUT
["a", "b"]
# PROBLEMS
NIL
