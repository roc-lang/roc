# META
~~~ini
description=List.count_if returns 0 when no elements match
type=repl
~~~
# SOURCE
~~~roc
» List.count_if([1, 2, 3], |x| x > 10)
~~~
# OUTPUT
0
# PROBLEMS
NIL
