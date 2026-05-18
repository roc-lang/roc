# META
~~~ini
description=Dict.to_list returns the key-value pairs as a list of tuples
type=repl
~~~
# SOURCE
~~~roc
» Dict.empty().insert("a", 1).insert("b", 2).to_list()
~~~
# OUTPUT
[("a", 1.0), ("b", 2.0)]
# PROBLEMS
NIL
