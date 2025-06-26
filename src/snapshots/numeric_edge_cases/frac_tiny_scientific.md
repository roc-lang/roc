# META
~~~ini
description=Very small number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e-100
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-9 (raw "1.0e-100"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 @1-1-1-9 (frac-var 74) (fits-in-f32 "false") (fits-in-dec "false") (value "1e-100") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Fraction(*))"))
~~~