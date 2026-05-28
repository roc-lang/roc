# META
~~~ini
description=Number with type suffix that is not in scope
type=expr
~~~
# SOURCE
~~~roc
0.F
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-typed-int (raw "0") (type "F"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "undeclared_type"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
