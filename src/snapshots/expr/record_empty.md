# META
~~~ini
description=Empty record expression
type=expr
~~~
# SOURCE
~~~roc
{}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-record @1-1-1-3)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# TYPES
~~~clojure
(expr (id 72) (type "{}"))
~~~