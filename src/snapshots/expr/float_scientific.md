# META
~~~ini
description=Scientific notation float literal
type=expr
~~~
# SOURCE
~~~roc
1.23e-4
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-8 (raw "1.23e-4"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 @1-1-1-8 (frac-var 74) (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(FloatingPoint(*))"))
~~~