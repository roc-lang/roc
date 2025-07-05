# META
~~~ini
description=basic_var
type=expr
~~~
# SOURCE
~~~roc
whee
~~~
# EXPECTED
UNDEFINED VARIABLE - basic_var.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "whee"))
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
(expr @1.1-1.5 (type "Error"))
~~~
