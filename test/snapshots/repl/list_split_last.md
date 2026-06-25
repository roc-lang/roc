# META
~~~ini
description=List.split_last splits a list at the last occurrence of the delimiter element
type=repl
~~~
# SOURCE
~~~roc
» List.split_last([1, 2, 3, 2, 4], 2)
~~~
# OUTPUT
Ok({ after: [4.0], before: [1.0, 2.0, 3.0] })
# PROBLEMS
NIL
