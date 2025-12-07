# META
~~~ini
description=List.count_if on empty list returns 0
type=repl
~~~
# SOURCE
~~~roc
» List.count_if([], |x| x > 2)
~~~
# OUTPUT
0
# PROBLEMS
NIL
