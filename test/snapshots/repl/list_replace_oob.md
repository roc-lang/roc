# META
~~~ini
description=List.replace on out-of-bounds index returns Err(OutOfBounds)
type=repl
~~~
# SOURCE
~~~roc
» List.replace([10, 20, 30], 5, 99)
~~~
# OUTPUT
Err(OutOfBounds)
# PROBLEMS
NIL
