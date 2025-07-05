# META
~~~ini
description=paren_newline_before_return
type=expr
~~~
# SOURCE
~~~roc
(i

)
return u
~~~
# EXPECTED
UNDEFINED VARIABLE - paren_newline_before_return.md:1:2:1:3
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:1-1:1),
Newline(1:1-1:1),
CloseRound(3:1-3:2),Newline(1:1-1:1),
KwReturn(4:1-4:7),LowerIdent(4:8-4:9),EndOfFile(4:9-4:9),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-3.2
	(e-ident @1.2-1.3 (raw "i")))
~~~
# FORMATTED
~~~roc
(
	i,

)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.2-1.3 (type "Error"))
~~~
