# META
~~~ini
description=List.count_if counts elements where predicate returns true
type=repl
~~~
# SOURCE
~~~roc
» List.count_if([1, 2, 3, 4, 5], |x| x > 2)
~~~
# OUTPUT
3
# PROBLEMS
NIL
