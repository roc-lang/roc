# META
~~~ini
description=Negative float literal
type=expr
~~~
# SOURCE
~~~roc
-2.5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.5 (raw "-2.5"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.5 (numerator "-25") (denominator-power-of-ten "1") (value "-2.5"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Num(Frac(_size))"))
~~~
