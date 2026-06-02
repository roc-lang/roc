# META
~~~ini
description=List.fold_with_index_until stops once the step function returns Break
type=repl
~~~
# SOURCE
~~~roc
» List.fold_with_index_until([10, 20, 30, 40], 0, |acc, x, i| if i >= 2 { Break(acc) } else { Continue(acc + x) })
~~~
# OUTPUT
30.0
# PROBLEMS
NIL
