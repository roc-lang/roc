# META
~~~ini
description=Simple integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-3 (raw "42"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-3 (value "42") (id 72))
~~~
# TYPES
~~~clojure
(expr (id 72) (type "Num(*)"))
~~~