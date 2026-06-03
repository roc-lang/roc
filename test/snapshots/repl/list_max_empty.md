# META
~~~ini
description=List.max on an empty list returns Err(ListWasEmpty)
type=repl
~~~
# SOURCE
~~~roc
» List.max(List.drop_first([1, 2, 3], 5))
~~~
# OUTPUT
Err(ListWasEmpty)
# PROBLEMS
NIL
