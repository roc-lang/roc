# META
~~~ini
description=For loop with complex var mutation
type=repl
~~~
# SOURCE
~~~roc
» countEvens = { var count_ = 0; var sum_ = 0; for n in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] { if n % 2 == 0 { count_ = count_ + 1; sum_ = sum_ + n } else { {} } }; count_ * sum_ }
~~~
# OUTPUT
assigned `countEvens`
# PROBLEMS
NIL
