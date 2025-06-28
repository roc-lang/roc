# META
~~~ini
description=Integer zero
type=expr
~~~
# SOURCE
~~~roc
0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),EndOfFile(1:2-1:2),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-2 (raw "0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-2 (value "0") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
