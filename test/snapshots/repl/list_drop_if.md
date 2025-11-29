# META
~~~ini
description=List.drop_if filters elements where predicate returns false
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.drop_if([1, 2, 3, 4, 5], |x| x > 2))
~~~
# OUTPUT
2
# PROBLEMS
NIL
