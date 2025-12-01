# META
~~~ini
description=Polymorphic sum function with U64 type inference
type=repl
~~~
# SOURCE
~~~roc
» sum = |a, b| a + b + 0
» U64.to_str(sum(240, 20))
» U64.to_str(sum(240, 0))
~~~
# OUTPUT
assigned `sum`
---
"260"
---
"240"
# PROBLEMS
NIL
