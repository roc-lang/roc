# META
~~~ini
description=List.fold_rev with subtraction (order-dependent)
type=repl
~~~
# SOURCE
~~~roc
» List.fold_rev([1, 2, 3], 0, |x, acc| x - acc)
~~~
# OUTPUT
2
# PROBLEMS
NIL
