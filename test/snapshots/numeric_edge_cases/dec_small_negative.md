# META
~~~ini
description=Small negative decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
-3.14159
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
(e-frac (raw "-3.14159"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "-3.14159"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : (List(U8), List(U8)) -> Try(num, [OutOfRange])])"))
~~~
