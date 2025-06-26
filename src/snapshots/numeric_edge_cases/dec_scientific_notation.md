# META
~~~ini
description=Dec literal with scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e10
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:23),EndOfFile(1:23-1:23),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-23 (raw "1.23456789012345678e10"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1-1-1-23 (frac-var 74) (fits-in-f32 "true") (fits-in-dec "true") (value "1.2345678901234568e10") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Fraction(*))"))
~~~