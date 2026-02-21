# META
~~~ini
description=Try.ok_or with Ok and Err variants
type=repl
~~~
# SOURCE
~~~roc
» Try.ok_or(Try.Ok(42), 0)
» Try.ok_or(Try.Err("oops"), 99)
~~~
# OUTPUT
42
---
99
# PROBLEMS
NIL
