# META
~~~ini
description=Basic List.fold_rev - demonstrates right-to-left iteration
type=repl
~~~
# SOURCE
~~~roc
» List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)
~~~
# OUTPUT
321.0
# PROBLEMS
NIL
