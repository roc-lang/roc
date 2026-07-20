# META
~~~ini
description=Try.map2 combining two results
type=repl
~~~
# SOURCE
~~~roc
» Try.map2(Try.Ok(2), Try.Ok(3), |a, b| a * b)
» Try.map2!(Try.Ok(2), Try.Ok(3), |a, b| a * b)
» Try.map2(Try.Err("first"), Try.Ok(3), |a, b| a * b)
» Try.map2(Try.Ok(2), Try.Err("second"), |a, b| a * b)
» Try.map2(Try.Err("first"), Try.Err("second"), |a, b| a * b)
» Try.map2!(Try.Err("first"), Try.Err("second"), |a, b| a * b)
» a = Try.Ok("a string long enough to be heap-allocated instead of stored inline")
» Try.map2(a, Try.Ok("another heap-allocated string that will not fit inline either"), |x, y| Str.concat(x, y))
» a
~~~
# OUTPUT
Ok(6.0)
---
Ok(6.0)
---
Err("first")
---
Err("second")
---
Err("first")
---
Err("first")
---
assigned `a`
---
Ok("a string long enough to be heap-allocated instead of stored inlineanother heap-allocated string that will not fit inline either")
---
Ok("a string long enough to be heap-allocated instead of stored inline")
# PROBLEMS
NIL
