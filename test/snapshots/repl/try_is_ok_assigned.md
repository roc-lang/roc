# META
~~~ini
description=Try.is_ok with assigned variable
type=repl
~~~
# SOURCE
~~~roc
» ok_val = Ok(42)
» Try.is_ok(ok_val)
~~~
# OUTPUT
assigned `ok_val`
---
True
# PROBLEMS
NIL
