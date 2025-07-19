# META
~~~ini
description=Unbound number types (Num, Int, Frac) should not display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» 42
» 3.14
» 1 + 2
~~~
# EXPECTED
42
---
Evaluation error: error.LayoutError
---
3
# PROBLEMS
NIL
