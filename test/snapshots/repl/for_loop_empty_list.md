# META
~~~ini
description=For loop with empty list
type=repl
~~~
# SOURCE
~~~roc
» unchanged = { var value_ = 42; for n in [] { value_ = n }; value_ }
~~~
# OUTPUT
assigned `unchanged`
# PROBLEMS
NIL
