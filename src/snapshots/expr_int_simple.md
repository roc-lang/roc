# META
~~~ini
description=Simple integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.3 (raw "42"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.3 (value "42"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Num(_size)"))
~~~
