# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
Ok
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.3 (raw "Ok"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.3 (name "Ok"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "[Ok]*"))
~~~
