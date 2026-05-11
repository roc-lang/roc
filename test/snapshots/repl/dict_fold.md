# META
~~~ini
description=Dict.fold accumulates a value across every key-value pair
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).insert("c", 3).fold(0, |acc, _k, v| acc + v)
~~~
# OUTPUT
6.0
# PROBLEMS
NIL
