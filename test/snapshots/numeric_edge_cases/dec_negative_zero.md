# META
~~~ini
description=Negative zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
-0.0
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
(e-frac (raw "-0.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "0") (denominator-power-of-ten "0") (value "0.0"))
~~~
# TYPES
~~~clojure
(expr (type "Num(Frac(_size))"))
~~~
