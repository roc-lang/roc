# META
~~~ini
description=List.min on an empty list returns Err(ListWasEmpty)
type=repl
~~~
# SOURCE
~~~roc
» List.min(List.drop_first([1, 2, 3], 5))
~~~
# OUTPUT
Err(ListWasEmpty)
# PROBLEMS
NIL
