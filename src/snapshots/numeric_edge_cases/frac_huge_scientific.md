# META
~~~ini
description=Very large number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e100
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-8 (raw "1.0e100"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 @1-1-1-8 (frac-var 74) (fits-in-f32 "false") (fits-in-dec "false") (value "1e100") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(FloatingPoint(*))"))
~~~