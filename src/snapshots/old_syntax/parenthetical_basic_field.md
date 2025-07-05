# META
~~~ini
description=parenthetical_basic_field
type=expr
~~~
# SOURCE
~~~roc
(rec).field
~~~
# EXPECTED
UNDEFINED VARIABLE - parenthetical_basic_field.md:1:2:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:5),CloseRound(1:5-1:6),NoSpaceDotLowerIdent(1:6-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.12
	(e-tuple @1.1-1.6
		(e-ident @1.2-1.5 (raw "rec")))
	(e-ident @1.6-1.12 (raw "field")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.12 (field "field")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.12 (type "*"))
~~~
