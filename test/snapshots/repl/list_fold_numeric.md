# META
~~~ini
description=List.fold with numeric accumulator should sum correctly
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3, 4, 5].fold(0, |acc, n| acc + n)
~~~
# OUTPUT
15
# PROBLEMS
NIL
