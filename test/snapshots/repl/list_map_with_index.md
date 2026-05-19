# META
~~~ini
description=List.map_with_index passes the element index to the transform function
type=repl
~~~
# SOURCE
~~~roc
» List.map_with_index([10, 20, 30], |x, i| x + i)
~~~
# OUTPUT
[10, 21, 32]
# PROBLEMS
NIL
