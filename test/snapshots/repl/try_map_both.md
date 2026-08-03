# META
~~~ini
description=Try.map_both with Ok and Err variants
type=repl
~~~
# SOURCE
~~~roc
» Try.map_both(Try.Err("failed"), |val| val + 1, |_| Failed)
» Try.map_both!(Try.Err("failed"), |val| val + 1, |_| Failed)
» Try.map_both(Try.Ok(100), |val| val - 50, |_| Failed)
» Try.map_both!(Try.Ok(100), |val| val - 50, |_| Failed)
» t = Try.Ok("a string long enough to be heap-allocated instead of stored inline")
» Try.map_both(t, |s| s, |e| e)
» t
~~~
# OUTPUT
Err(Failed)
---
Err(Failed)
---
Ok(50.0)
---
Ok(50.0)
---
assigned `t`
---
Ok("a string long enough to be heap-allocated instead of stored inline")
---
Ok("a string long enough to be heap-allocated instead of stored inline")
# PROBLEMS
NIL
