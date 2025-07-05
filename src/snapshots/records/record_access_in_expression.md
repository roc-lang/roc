# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
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
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:11),OpPlus(1:12-1:13),Int(1:14-1:15),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "+")
	(e-field-access @1.1-1.13
		(e-ident @1.1-1.7 (raw "person"))
		(e-ident @1.7-1.11 (raw "age")))
	(e-int @1.14-1.15 (raw "5")))
~~~
# FORMATTED
~~~roc
person.age + 5
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "add")
	(e-dot-access @1.1-1.13 (field "age")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope"))))
	(e-int @1.14-1.15 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
