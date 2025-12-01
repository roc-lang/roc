# META
~~~ini
description=Numeric without annotation, single use infers type from usage
type=repl
~~~
# SOURCE
~~~roc
» x = 42
» I64.to_str(x)
~~~
# OUTPUT
assigned `x`
---
"42"
# PROBLEMS
NIL
