# META
~~~ini
description=List.starts_with with an empty prefix is always True (every list starts with the empty list)
type=repl
~~~
# SOURCE
~~~roc
» List.starts_with([1, 2, 3], List.drop_first([1], 1))
~~~
# OUTPUT
True
# PROBLEMS
NIL
