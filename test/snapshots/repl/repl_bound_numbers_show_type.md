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
# CANONICALIZE
~~~clojure
(e-int @1.1-1.1 (value "42"))
(e-int @1.2-1.7 (value "42"))
---
(e-frac-f64 @1.1-1.1 (value "3.14"))
(e-frac-f64 @1.2-1.9 (value "3.14"))
---
(e-frac-dec @1.1-1.1 (value "1.5"))
(e-frac-dec @1.2-1.8 (value "1.5"))
---
(e-int @1.1-1.1 (value "255"))
(e-int @1.2-1.7 (value "255"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "I32"))
(expr @1.2-1.7 (type "I32"))
---
(expr @1.1-1.1 (type "F64"))
(expr @1.2-1.9 (type "F64"))
---
(expr @1.1-1.1 (type "Dec"))
(expr @1.2-1.8 (type "Dec"))
---
(expr @1.1-1.1 (type "U8"))
(expr @1.2-1.7 (type "U8"))
~~~
