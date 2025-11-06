# META
~~~ini
description=Scientific notation float literal
type=expr
~~~
# SOURCE
~~~roc
1.23e-4
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "1.23e-4"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "0.000123"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(num, [OutOfRange])])"))
~~~
