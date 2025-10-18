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
OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-empty_record)
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
