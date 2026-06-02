# META
~~~ini
description=List.fold_with_index_until accumulates Continue states with index access
type=repl
~~~
# SOURCE
~~~roc
» List.fold_with_index_until([10, 20, 30, 40], 0, |acc, x, i| Continue(acc + x * i))
~~~
# OUTPUT
200
# PROBLEMS
NIL
