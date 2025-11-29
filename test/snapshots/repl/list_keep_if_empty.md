# META
~~~ini
description=List.keep_if on empty list returns empty list
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.keep_if([1, 2, 3], |_| Bool.False))
~~~
# OUTPUT
0
# PROBLEMS
NIL
