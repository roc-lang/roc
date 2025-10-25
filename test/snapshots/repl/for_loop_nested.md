# META
~~~ini
description=Nested for loops
type=repl
~~~
# SOURCE
~~~roc
» product = { var result_ = 0; for i in [1, 2, 3] { for j in [10, 20] { result_ = result_ + (i * j) } }; result_ }
~~~
# OUTPUT
assigned `product`
# PROBLEMS
NIL
