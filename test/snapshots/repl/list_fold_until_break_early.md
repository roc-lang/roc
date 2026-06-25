# META
~~~ini
description=List.fold_until stops once the step function returns Break, returning that state
type=repl
~~~
# SOURCE
~~~roc
» List.fold_until([1, 2, 3, 4, 5], 0, |acc, x| if acc + x > 5 { Break(acc) } else { Continue(acc + x) })
~~~
# OUTPUT
3.0
# PROBLEMS
NIL
