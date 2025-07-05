# META
~~~ini
description=elm_function_syntax fail
type=expr
~~~
# SOURCE
~~~roc
f x y = x
~~~
# EXPECTED
UNDEFINED VARIABLE - elm_function_syntax.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),LowerIdent(1:3-1:4),LowerIdent(1:5-1:6),OpAssign(1:7-1:8),LowerIdent(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
