# META
~~~ini
description=List.split_at with index 0 places the entire list in the second half
type=repl
~~~
# SOURCE
~~~roc
» List.split_at([1, 2, 3], 0)
~~~
# OUTPUT
{ before: [], others: [1.0, 2.0, 3.0] }
# PROBLEMS
NIL
