# META
~~~ini
description=List.keep_until keeps every element until predicate returns true
type=repl
~~~
# SOURCE
~~~roc
» List.keep_until([1, 2, 3], |_| Bool.True)
~~~
# OUTPUT
[]
# PROBLEMS
NIL
