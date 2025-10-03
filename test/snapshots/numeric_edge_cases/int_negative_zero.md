# META
~~~ini
description=Negative integer zero
type=expr
~~~
# SOURCE
~~~roc
-0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.3 (raw "-0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num @1.1-1.3 (value "0"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Num(_size)"))
~~~
