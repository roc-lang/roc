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
# OUTPUT
42
---
3.14e0
---
1.5e0
---
255
# PROBLEMS
NIL
