# META
~~~ini
description=List.set with an out-of-bounds index returns the list unchanged
type=repl
~~~
# SOURCE
~~~roc
» List.set([10, 20, 30], 5, 99)
~~~
# OUTPUT
[10.0, 20.0, 30.0]
# PROBLEMS
NIL
