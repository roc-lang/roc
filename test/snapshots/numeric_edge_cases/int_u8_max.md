# META
~~~ini
description=Maximum value for u8 (255)
type=expr
~~~
# SOURCE
~~~roc
255
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
(e-int (raw "255"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "255"))
~~~
# TYPES
~~~clojure
(expr (type "num where [num.from_int_digits : List(U8) -> Try(num, [OutOfRange])]"))
~~~
