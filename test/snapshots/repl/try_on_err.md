# META
~~~ini
description=Try.on_err recovering from an Err
type=repl
~~~
# SOURCE
~~~roc
» Try.on_err(Try.Err("failed"), |_| Try.Ok(0))
» Try.on_err!(Try.Err("failed"), |_| Try.Ok(0))
» Try.on_err(Try.Ok(7), |_| Try.Ok(0))
» Try.on_err!(Try.Ok(7), |_| Try.Ok(0))
» Try.on_err(Try.Err("first"), |_| Try.Err("recovery also failed"))
» e = Try.Err("a string long enough to be heap-allocated instead of stored inline")
» Try.on_err(e, |s| Try.Err(s))
» e
~~~
# OUTPUT
Ok(0.0)
---
Ok(0.0)
---
Ok(7.0)
---
Ok(7.0)
---
Err("recovery also failed")
---
assigned `e`
---
Err("a string long enough to be heap-allocated instead of stored inline")
---
Err("a string long enough to be heap-allocated instead of stored inline")
# PROBLEMS
NIL
