# META
~~~ini
description=List.fold_with_index on an empty list returns the initial state unchanged
type=repl
~~~
# SOURCE
~~~roc
» List.fold_with_index(List.drop_first([1], 1), 42, |acc, x, i| acc + x + i)
~~~
# OUTPUT
42
# PROBLEMS
NIL
