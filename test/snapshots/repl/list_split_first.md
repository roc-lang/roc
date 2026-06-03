# META
~~~ini
description=List.split_first splits a list at the first occurrence of the delimiter element
type=repl
~~~
# SOURCE
~~~roc
» List.split_first([1, 2, 3, 2, 4], 2)
~~~
# OUTPUT
Ok({ after: [3.0, 2.0, 4.0], before: [1.0] })
# PROBLEMS
NIL
