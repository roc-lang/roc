# META
~~~ini
description=Bound number types (I32, F64, Dec, etc.) should display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» 42.I32
» 3.14.F64
» 1.5.Dec
» 255.U8
~~~
# OUTPUT
42
---
3.14
---
1.5
---
255
# PROBLEMS
NIL
