# META
~~~ini
description=Scientific notation uses Dec when value fits
type=expr
~~~
# SOURCE
~~~roc
1.5e2
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
(e-frac (raw "1.5e2"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "150") (denominator-power-of-ten "0") (value "150"))
~~~
# TYPES
~~~clojure
(expr (type "Num(Frac(_size))"))
~~~
