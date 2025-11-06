# META
~~~ini
description=Minimum negative value that fits in dec_small (i16 min)
type=expr
~~~
# SOURCE
~~~roc
-327.68
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
(e-frac (raw "-327.68"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "-32768") (denominator-power-of-ten "2") (value "-327.68"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(num, [OutOfRange])])"))
~~~
