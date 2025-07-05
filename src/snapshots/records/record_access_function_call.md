# META
~~~ini
description=Record field access with function call
type=expr
~~~
# SOURCE
~~~roc
(person.transform)(42)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:8),NoSpaceDotLowerIdent(1:8-1:18),CloseRound(1:18-1:19),NoSpaceOpenRound(1:19-1:20),Int(1:20-1:22),CloseRound(1:22-1:23),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.23
	(e-tuple @1.1-1.19
		(e-field-access @1.2-1.19
			(e-ident @1.2-1.8 (raw "person"))
			(e-ident @1.8-1.18 (raw "transform"))))
	(e-int @1.20-1.22 (raw "42")))
~~~
# FORMATTED
~~~roc
(person.transform)(42)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.23
	(e-dot-access @1.2-1.19 (field "transform")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope"))))
	(e-int @1.20-1.22 (value "42")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.23 (type "*"))
~~~
