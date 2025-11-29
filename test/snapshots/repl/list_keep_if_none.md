# META
~~~ini
description=List.keep_if that keeps no elements returns empty list
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.keep_if([1, 2, 3], |x| x > 10))
~~~
# OUTPUT
0
# PROBLEMS
NIL
