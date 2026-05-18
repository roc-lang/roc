# META
~~~ini
description=Dict.values returns the values as a list
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).values()
~~~
# OUTPUT
[1.0, 2.0]
# PROBLEMS
NIL
