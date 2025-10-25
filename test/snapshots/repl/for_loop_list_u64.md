# META
~~~ini
description=For loop iterating over List U64
type=repl
~~~
# SOURCE
~~~roc
» sum = { var total_ = 0; for n in [1, 2, 3, 4, 5] { total_ = total_ + n }; total_ }
~~~
# OUTPUT
assigned `sum`
# PROBLEMS
NIL
