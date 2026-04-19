# META
~~~ini
description=Chained record field (field-access)
type=expr
~~~
# SOURCE
~~~roc
person.address.street
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-field-access
		(e-ident (raw "person"))
		(e-ident (raw "address")))
	(e-ident (raw "street")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-field-access (field "street")
	(receiver
		(e-field-access (field "address")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
