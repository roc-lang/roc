# META
~~~ini
description=Decimal with 8 decimal places
type=expr
~~~
# SOURCE
~~~roc
3.14159265
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
(e-frac (raw "3.14159265"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "3.14159265"))
~~~
# TYPES
~~~clojure
(expr (type "Num(num where [num.from_dec_digits : (List(U8), List(U8)) -> Try(num, [OutOfRange])])"))
~~~
