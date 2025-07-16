# META
~~~ini
description=Bound number types (I32, F64, Dec, etc.) should display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» 42i32
» 3.14f64
» 1.5dec
» 255u8
~~~
# EXPECTED
Evaluation error: error.Crash
---
Failed to canonicalize expression
---
Evaluation error: error.Crash
---
Evaluation error: error.Crash
# PROBLEMS
NIL
