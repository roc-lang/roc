# META
~~~ini
description=List.set with an out-of-bounds index returns Err(OutOfBounds)
type=repl
~~~
# SOURCE
~~~roc
» List.set([10, 20, 30], 5, 99)
~~~
# OUTPUT
Err(OutOfBounds)
# PROBLEMS
NIL
