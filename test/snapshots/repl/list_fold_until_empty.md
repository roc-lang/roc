# META
~~~ini
description=List.fold_until on an empty list returns the initial state unchanged
type=repl
~~~
# SOURCE
~~~roc
» List.fold_until([], 42, |acc, x| Continue(acc + x))
~~~
# OUTPUT
42.0
# PROBLEMS
NIL
