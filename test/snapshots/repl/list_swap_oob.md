# META
~~~ini
description=List.swap with an out-of-bounds index returns the list unchanged
type=repl
~~~
# SOURCE
~~~roc
» List.swap([10, 20, 30], 0, 5)
~~~
# OUTPUT
[10.0, 20.0, 30.0]
# PROBLEMS
NIL
