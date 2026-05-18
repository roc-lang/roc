# META
~~~ini
description=List.update with an out-of-bounds index returns the list unchanged
type=repl
~~~
# SOURCE
~~~roc
» List.update([10, 20, 30], 5, |x| x + 5)
~~~
# OUTPUT
[10.0, 20.0, 30.0]
# PROBLEMS
NIL
