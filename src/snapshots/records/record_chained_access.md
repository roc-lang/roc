# META
~~~ini
description=Chained record field (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.address.street
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:15),NoSpaceDotLowerIdent(1:15-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.22
	(e-field-access @1.1-1.22
		(e-ident @1.1-1.7 (qaul "") (raw "person"))
		(e-ident @1.7-1.15 (qaul "") (raw ".address")))
	(e-ident @1.15-1.22 (qaul "") (raw ".street")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.22 (field "street") (id 76)
	(receiver
		(e-dot-access @1.1-1.22 (field "address")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~
