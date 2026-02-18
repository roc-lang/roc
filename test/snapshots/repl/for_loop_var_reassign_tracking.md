# META
~~~ini
description=For loop with var reassignment tracking across iterations
type=repl
~~~
# SOURCE
~~~roc
» result = { var sum_ = 0; var max_ = 0; for n in [3, 7, 2, 9, 1] { sum_ = sum_ + n; if n > max_ { max_ = n } else { {} } }; sum_ + max_ }
~~~
# OUTPUT
assigned `result`
# PROBLEMS
NIL
