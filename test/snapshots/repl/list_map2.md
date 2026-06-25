# META
~~~ini
description=List.map2 applies a binary function pairwise over two lists
type=repl
~~~
# SOURCE
~~~roc
» List.map2([1, 2, 3], [10, 20, 30], |a, b| a + b)
~~~
# OUTPUT
[11.0, 22.0, 33.0]
# PROBLEMS
NIL
