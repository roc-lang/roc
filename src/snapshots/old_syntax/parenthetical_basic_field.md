# META
~~~ini
description=parenthetical_basic_field
type=expr
~~~
# SOURCE
~~~roc
(rec).field
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `rec` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:5),CloseRound(1:5-1:6),NoSpaceDotLowerIdent(1:6-1:12),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-2.2
	(e-tuple @1.1-1.6
		(e-ident @1.2-1.5 (raw "rec")))
	(e-ident @1.6-1.12 (raw "field")))
~~~
# FORMATTED
~~~roc
(rec).field
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-2.2 (field "field")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
