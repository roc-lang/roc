# META
~~~ini
description=Empty record expression
type=expr
~~~
# SOURCE
~~~roc
{}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.3)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-empty_record @1.1-1.3)
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "{}"))
~~~
