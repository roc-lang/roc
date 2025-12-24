# META
~~~ini
description=Try equality comparisons using == and !=
type=repl
~~~
# SOURCE
~~~roc
» Try.Ok(1) == Try.Ok(1)
» Try.Ok(1) == Try.Ok(2)
» Try.Ok(1) != Try.Ok(1)
» Try.Ok(1) != Try.Ok(2)
~~~
# OUTPUT
True
---
False
---
False
---
True
# PROBLEMS
NIL
