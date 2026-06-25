# META
~~~ini
description=List.fold_with_index passes the element index to the step function
type=repl
~~~
# SOURCE
~~~roc
» List.fold_with_index([10, 20, 30], 0, |acc, x, i| acc + x * i)
~~~
# OUTPUT
80
# PROBLEMS
NIL
