# META
~~~ini
description=Decimal that is exactly 1/10^n
type=expr
~~~
# SOURCE
~~~roc
0.001
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
(e-frac (raw "0.001"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "1") (denominator-power-of-ten "3") (value "0.001"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : (List(U8), List(U8)) -> Try(num, [OutOfRange])])"))
~~~
