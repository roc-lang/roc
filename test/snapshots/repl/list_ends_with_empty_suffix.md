# META
~~~ini
description=List.ends_with with an empty suffix is always True (every list ends with the empty list)
type=repl
~~~
# SOURCE
~~~roc
» List.ends_with([1, 2, 3], List.drop_first([1], 1))
~~~
# OUTPUT
True
# PROBLEMS
NIL
