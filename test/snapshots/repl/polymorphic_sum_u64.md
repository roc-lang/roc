# META
~~~ini
description=Polymorphic sum function with U64 type inference
type=repl
~~~
# SOURCE
~~~roc
» sum = |num| 0 + num
» U64.to_str(sum(2400))
~~~
# OUTPUT
assigned `sum`
---
"2400"
# PROBLEMS
NIL
