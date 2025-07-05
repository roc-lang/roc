# META
~~~ini
description=f_not_not_f
type=expr
~~~
# SOURCE
~~~roc
f
!
!f
~~~
# EXPECTED
UNDEFINED VARIABLE - f_not_not_f.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpBang(2:1-2:2),Newline(1:1-1:1),
OpBang(3:1-3:2),LowerIdent(3:2-3:3),EndOfFile(3:3-3:3),
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
