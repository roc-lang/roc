# META
~~~ini
description=where_clause_non_function
type=expr
~~~
# SOURCE
~~~roc
f : a where a implements A

f
~~~
# EXPECTED
UNDEFINED VARIABLE - where_clause_non_function.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),LowerIdent(1:5-1:6),KwWhere(1:7-1:12),LowerIdent(1:13-1:14),KwImplements(1:15-1:25),UpperIdent(1:26-1:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
