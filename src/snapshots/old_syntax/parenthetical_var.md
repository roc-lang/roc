# META
~~~ini
description=parenthetical_var
type=expr
~~~
# SOURCE
~~~roc
(whee)
~~~
# EXPECTED
UNDEFINED VARIABLE - parenthetical_var.md:1:2:1:6
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:6),CloseRound(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.7
	(e-ident @1.2-1.6 (raw "whee")))
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
(expr @1.2-1.6 (type "Error"))
~~~
