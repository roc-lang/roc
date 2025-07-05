# META
~~~ini
description=call_bang_no_space
type=expr
~~~
# SOURCE
~~~roc
fxFn!arg
~~~
# EXPECTED
UNDEFINED VARIABLE - call_bang_no_space.md:1:1:1:9
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.9 (raw "fxFn!arg"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "Error"))
~~~
