# META
~~~ini
description=Very large number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e100
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
(e-frac (raw "1.0e100"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 (value "1e100"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : (List(U8), List(U8)) -> Try(num, [OutOfRange])])"))
~~~
