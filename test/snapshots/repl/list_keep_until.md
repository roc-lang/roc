# META
~~~ini
description=List.keep_until keeps every element until predicate returns true
type=repl
~~~
# SOURCE
~~~roc
» keep_until([1, 2, 3], |item| item > 2)
~~~
# OUTPUT
[1.0, 2.0]
# PROBLEMS
NIL
