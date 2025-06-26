# META
~~~ini
description=Record field access with function calls (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.transform(42)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:17),NoSpaceOpenRound(1:17-1:18),Int(1:18-1:20),CloseRound(1:20-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(e-field-access @1-1-1-21
	(e-binop @1-1-1-21 (op "person")
		(e-ident @1-1-1-7 (qaul "") (raw "person"))
		(e-apply @1-7-1-21
			(e-ident @1-7-1-17 (qaul "") (raw ".transform"))
			(e-int @1-18-1-20 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~