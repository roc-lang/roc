# META
~~~ini
description=List.keep_while keeps every element until predicate returns false
type=repl
~~~
# SOURCE
~~~roc
» List.keep_while([1, 2, 3], |item| item < 3)
~~~
# OUTPUT
[1.0, 2.0]
# PROBLEMS
NIL
