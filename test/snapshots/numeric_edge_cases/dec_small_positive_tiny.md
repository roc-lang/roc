# META
~~~ini
description=Tiny positive decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
0.0001
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
(e-frac (raw "0.0001"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "1") (denominator-power-of-ten "4") (value "0.0001"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : (List(U8), List(U8)) -> Try(num, [OutOfRange])])"))
~~~
