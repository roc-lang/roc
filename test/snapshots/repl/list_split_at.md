# META
~~~ini
description=List.split_at splits a list into two parts at the given index
type=repl
~~~
# SOURCE
~~~roc
» List.split_at([0, 1, 2, 3, 4], 2)
~~~
# OUTPUT
{ before: [0.0, 1.0], others: [2.0, 3.0, 4.0] }
# PROBLEMS
NIL
