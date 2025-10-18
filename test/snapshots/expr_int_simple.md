# META
~~~ini
description=Simple integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
42
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
(e-int (raw "42"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "42"))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
