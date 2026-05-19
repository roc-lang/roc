# META
~~~ini
description=List.swap with an out-of-bounds index returns Err(OutOfBounds)
type=repl
~~~
# SOURCE
~~~roc
» List.swap([10, 20, 30], 0, 5)
~~~
# OUTPUT
Err(OutOfBounds)
# PROBLEMS
NIL
