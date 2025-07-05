# META
~~~ini
description=def_missing_final_expression fail
type=expr
~~~
# SOURCE
~~~roc
f : Foo.foo
~~~
# EXPECTED
UNDEFINED VARIABLE - def_missing_final_expression.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:8),NoSpaceDotLowerIdent(1:8-1:12),EndOfFile(1:12-1:12),
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
