# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `fn` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),NoSpaceDotLowerIdent(1:5-1:9),NoSpaceOpenRound(1:9-1:10),LowerIdent(1:10-1:12),CloseRound(1:12-1:13),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-2.2
	(e-ident @1.1-1.5 (raw "list"))
	(e-apply @1.5-1.13
		(e-ident @1.5-1.9 (raw "map"))
		(e-ident @1.10-1.12 (raw "fn"))))
~~~
# FORMATTED
~~~roc
list.map(fn)
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-2.2 (field "map")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
