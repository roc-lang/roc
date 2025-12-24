# META
~~~ini
description=List.keep_if on empty list returns empty list
type=repl
~~~
# SOURCE
~~~roc
» List.keep_if([1, 2, 3], |_| Bool.False)
~~~
# OUTPUT
[]
# PROBLEMS
NIL
