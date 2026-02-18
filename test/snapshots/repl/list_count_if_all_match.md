# META
~~~ini
description=List.count_if returns list length when all elements match
type=repl
~~~
# SOURCE
~~~roc
» List.count_if([1, 2, 3, 4, 5], |x| x > 0)
~~~
# OUTPUT
5
# PROBLEMS
NIL
