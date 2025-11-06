# META
~~~ini
description=Dec literal with scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e10
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
(e-frac (raw "1.23456789012345678e10"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "1.2345678901234568e10"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : { before_dot : List(U8), after_dot : List(U8) } -> Try(num, [OutOfRange])])"))
~~~
