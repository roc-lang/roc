# META
~~~ini
description=Decimal just above dec_small limit that requires frac_dec
type=expr
~~~
# SOURCE
~~~roc
32768.0
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
(e-frac (raw "32768.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "32768"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(num, [OutOfRange])])"))
~~~
