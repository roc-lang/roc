# META
~~~ini
description=List.swap with the same index for both args is a no-op
type=repl
~~~
# SOURCE
~~~roc
» List.swap([10, 20, 30], 1, 1)
~~~
# OUTPUT
Ok([10.0, 20.0, 30.0])
# PROBLEMS
NIL
