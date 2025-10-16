# META
~~~ini
description=Minimum value for i8 (-128)
type=expr
~~~
# SOURCE
~~~roc
-128
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "-128"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "-128"))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
