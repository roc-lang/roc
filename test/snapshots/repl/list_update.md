# META
~~~ini
description=List.update applies a function to the element at the given index
type=repl
~~~
# SOURCE
~~~roc
» List.update([10, 20, 30], 1, |x| x + 5)
~~~
# OUTPUT
Ok([10.0, 25.0, 30.0])
# PROBLEMS
NIL
