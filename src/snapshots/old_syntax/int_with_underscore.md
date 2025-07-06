# META
~~~ini
description=int_with_underscore
type=expr
~~~
# SOURCE
~~~roc
1__23
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.6 (raw "1__23"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.6 (value "123"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Num(*)"))
~~~
