# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-field-access @1-1-1-12
	(e-binop @1-1-1-12 (op "person")
		(e-ident @1-1-1-7 (qaul "") (raw "person"))
		(e-ident @1-7-1-12 (qaul "") (raw ".name"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1-1-1-12 (field "name") (id 74)
	(receiver
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "*"))
~~~