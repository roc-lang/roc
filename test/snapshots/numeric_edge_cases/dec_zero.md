# META
~~~ini
description=Zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
0.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:4),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.4 (raw "0.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.4 (numerator "0") (denominator-power-of-ten "1") (value "0.0"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(Frac(_size))"))
~~~
