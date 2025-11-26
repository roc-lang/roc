# META
~~~ini
description=Try equality comparisons using == and !=
type=repl
~~~
# SOURCE
~~~roc
» Try.Ok(1) == Try.Ok(1)
» Try.Ok(1) == Try.Ok(2)
» Try.Err("error") == Try.Err("error")
» Try.Ok(1) == Try.Err("error")
» Try.Ok(1) != Try.Ok(2)
» Try.Ok(1) != Try.Ok(1)
~~~
# OUTPUT
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
---
Crash: e_closure: failed to resolve capture value
# PROBLEMS
NIL
