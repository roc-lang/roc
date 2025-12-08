# META
~~~ini
description=Numeric without annotation, multiple uses with different types (each use gets fresh type)
type=repl
~~~
# SOURCE
~~~roc
» x = 42
» a = I64.to_str(x)
» b = Dec.to_str(x)
» Str.concat(a, b)
~~~
# OUTPUT
assigned `x`
---
assigned `a`
---
assigned `b`
---
"4242.0"
# PROBLEMS
NIL
