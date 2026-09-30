# META
~~~ini
description=List.keep_while keeps every element until predicate returns false
type=repl
~~~
# SOURCE
~~~roc
» keep_while([1, 2, 3], |_| Bool.False)
~~~
# OUTPUT
[]
# PROBLEMS
NIL
