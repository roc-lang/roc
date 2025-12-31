# META
~~~ini
description=Try.map_ok with Ok and Err variants
type=repl
~~~
# SOURCE
~~~roc
» Try.map_ok(Try.Err("failed"), |val| val + 1)
» Try.map_ok!(Try.Err("failed"), |val| val + 1)
» Try.map_ok(Try.Ok(100), |val| val - 50)
» Try.map_ok!(Try.Ok(100), |val| val - 50)
~~~
# OUTPUT
Err("failed")
---
Err("failed")
---
Ok(50)
---
Ok(50)
# PROBLEMS
NIL
