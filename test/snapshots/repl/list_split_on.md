# META
~~~ini
description=List.split_on splits a list into sublists at each occurrence of the delimiter element
type=repl
~~~
# SOURCE
~~~roc
» List.split_on([1, 2, 1, 2, 3], 1)
~~~
# OUTPUT
[[], [2.0], [2.0, 3.0]]
# PROBLEMS
NIL
