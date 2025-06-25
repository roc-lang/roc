# META
~~~ini
description=Simple float literal
type=expr
~~~
# SOURCE
~~~roc
3.14
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-5 (raw "3.14"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-5 (num-var 74) (fits-in-f32 "true") (fits-in-dec "true") (numerator "314") (denominator-power-of-ten "2") (value "3.14") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(FloatingPoint(*))"))
~~~