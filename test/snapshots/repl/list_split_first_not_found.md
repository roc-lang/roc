# META
~~~ini
description=List.split_first returns Err(NotFound) when the delimiter is absent
type=repl
~~~
# SOURCE
~~~roc
» List.split_first([1, 2, 3], 99)
~~~
# OUTPUT
Err(NotFound)
# PROBLEMS
NIL
