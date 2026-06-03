# META
~~~ini
description=List.fold_until returns the Break payload immediately when the first step breaks
type=repl
~~~
# SOURCE
~~~roc
» List.fold_until([10, 20, 30], 0, |acc, x| Break(acc + x))
~~~
# OUTPUT
10.0
# PROBLEMS
NIL
