# META
~~~ini
description=Try.err_or with Ok and Err variants
type=repl
~~~
# SOURCE
~~~roc
» Try.err_or(Try.Err("failed"), "default")
» Try.err_or(Try.Ok(123), "default")
~~~
# OUTPUT
"failed"
---
"default"
# PROBLEMS
NIL
