# META
~~~ini
description=List.keep_if filters elements where predicate returns true
type=repl
~~~
# SOURCE
~~~roc
» List.keep_if([1, 2, 3, 4, 5], |x| x > 2)
~~~
# OUTPUT
[3, 4, 5]
# PROBLEMS
NIL
