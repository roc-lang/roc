# META
~~~ini
description=For loop with var reassignment
type=repl
~~~
# SOURCE
~~~roc
» result = { var prev_ = 0; var count_ = 0; for n in [10, 20, 30, 40, 50] { count_ = count_ + 1; prev_ = n }; prev_ + count_ }
~~~
# OUTPUT
assigned `result`
# PROBLEMS
NIL
