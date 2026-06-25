# META
~~~ini
description=List.split_on preserves empty sublists between consecutive delimiters
type=repl
~~~
# SOURCE
~~~roc
» List.split_on([1, 1, 2, 1, 3], 1)
~~~
# OUTPUT
[[], [], [2.0], [3.0]]
# PROBLEMS
NIL
