# META
~~~ini
description=Issue 9377 - REPL must not panic when a static-dispatch call is assigned to a var and then evaluated
type=repl
~~~
# SOURCE
~~~roc
» [1,2,3].first()
» numbers = [1,2,3]
» numbers.first()
» out = numbers.first()
» out
~~~
# OUTPUT
Ok(1.0)
---
assigned `numbers`
---
Ok(1.0)
---
assigned `out`
---
Ok(1.0)
# PROBLEMS
NIL
