# META
~~~ini
description=For loop iterating over List Str
type=repl
~~~
# SOURCE
~~~roc
» count = { var counter_ = 0; for _ in ["hello", "world", "test"] { counter_ = counter_ + 1 }; counter_ }
~~~
# OUTPUT
assigned `count`
# PROBLEMS
NIL
