# META
~~~ini
description=List.replace on out-of-bounds index returns the original list and the input value
type=repl
~~~
# SOURCE
~~~roc
» List.replace([10, 20, 30], 5, 99)
~~~
# OUTPUT
{ list: [10.0, 20.0, 30.0], value: 99.0 }
# PROBLEMS
NIL
