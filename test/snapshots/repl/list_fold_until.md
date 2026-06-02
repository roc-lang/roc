# META
~~~ini
description=List.fold_until accumulates Continue states like List.fold when Break is never returned
type=repl
~~~
# SOURCE
~~~roc
» List.fold_until([1, 2, 3, 4], 0, |acc, x| Continue(acc + x))
~~~
# OUTPUT
10.0
# PROBLEMS
NIL
