# META
~~~ini
description=Double question operator with string values
type=repl
~~~
# SOURCE
~~~roc
» Try.Ok("hello") ?? "default"
» Try.Err(42) ?? "fallback"
~~~
# OUTPUT
"hello"
---
"fallback"
# PROBLEMS
NIL
