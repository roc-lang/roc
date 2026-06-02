# META
~~~ini
description=List.starts_with returns False when the prefix does not match
type=repl
~~~
# SOURCE
~~~roc
» List.starts_with([1, 2, 3], [9, 9])
~~~
# OUTPUT
False
# PROBLEMS
NIL
