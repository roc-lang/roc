# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
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
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:12),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-2.2
	(e-ident @1.1-1.7 (raw "person"))
	(e-ident @1.7-1.12 (raw "name")))
~~~
# FORMATTED
~~~roc
person.name
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-2.2 (field "name")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
