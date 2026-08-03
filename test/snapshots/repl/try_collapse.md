# META
~~~ini
description=Try.collapse on Ok and Err holding the same type
type=repl
~~~
# SOURCE
~~~roc
» Try.collapse(Try.Ok(5))
» Try.collapse(Try.Err(7))
» t = Try.Ok("a string long enough to be heap-allocated instead of stored inline")
» Try.collapse(t)
» t
~~~
# OUTPUT
5.0
---
7.0
---
assigned `t`
---
"a string long enough to be heap-allocated instead of stored inline"
---
Ok("a string long enough to be heap-allocated instead of stored inline")
# PROBLEMS
NIL
