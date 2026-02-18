# META
~~~ini
description=Chained double question operators with nested fallbacks
type=repl
~~~
# SOURCE
~~~roc
» Try.Err("first") ?? (Try.Err("second") ?? 42)
» Try.Err("first") ?? (Try.Ok(10) ?? 42)
» Try.Ok(5) ?? (Try.Ok(10) ?? 42)
~~~
# OUTPUT
42
---
10
---
5
# PROBLEMS
NIL
