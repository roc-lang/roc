# META
~~~ini
description=Try.catch collapsing both variants to one type
type=repl
~~~
# SOURCE
~~~roc
» Try.catch(Try.Err("failed"), |_| 0, |val| val * 2)
» Try.catch!(Try.Err("failed"), |_| 0, |val| val * 2)
» Try.catch(Try.Ok(12), |_| 0, |val| val * 2)
» Try.catch!(Try.Ok(12), |_| 0, |val| val * 2)
» t = Try.Ok("a string long enough to be heap-allocated instead of stored inline")
» Try.catch(t, |x| x, |x| x)
» t
~~~
# OUTPUT
0.0
---
0.0
---
24.0
---
24.0
---
assigned `t`
---
"a string long enough to be heap-allocated instead of stored inline"
---
Ok("a string long enough to be heap-allocated instead of stored inline")
# PROBLEMS
NIL
