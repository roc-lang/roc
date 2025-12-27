# META
~~~ini
description=Try.map_err with Ok and Err variants
type=repl
~~~
# SOURCE
~~~roc
» Try.map_err(Try.Err(50), |err_code| err_code + 1)
» Try.map_err!(Try.Err(50), |err_code| err_code + 1)
» Try.map_err(Try.Ok("hello"), |_| "world")
» Try.map_err!(Try.Ok("hello"), |_| "world")
~~~
# OUTPUT
Err(51)
---
Err(51)
---
Ok("hello")
---
Ok("hello")
# PROBLEMS
NIL
