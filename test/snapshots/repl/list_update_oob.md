# META
~~~ini
description=List.update with an out-of-bounds index returns Err(OutOfBounds)
type=repl
~~~
# SOURCE
~~~roc
» List.update([10, 20, 30], 5, |x| x + 5)
~~~
# OUTPUT
Err(OutOfBounds)
# PROBLEMS
NIL
