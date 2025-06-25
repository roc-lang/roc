# META
~~~ini
description=Negative integer zero
type=expr
~~~
# SOURCE
~~~roc
-0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-3 (raw "-0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-3 (num-var 74) (sign-needed "true") (bits-needed "7") (value "0") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~