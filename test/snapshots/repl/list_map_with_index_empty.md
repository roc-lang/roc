# META
~~~ini
description=List.map_with_index on an empty list returns the empty list
type=repl
~~~
# SOURCE
~~~roc
» List.map_with_index(List.drop_first([1], 1), |x, i| x + i)
~~~
# OUTPUT
[]
# PROBLEMS
NIL
