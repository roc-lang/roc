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
	(receiver
		(e-ident (raw "person")))
	(segment (mode "required") (field "address"))
	(segment (mode "required") (field "street")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-field-access
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(segments
		(segment (name "address") (mode "required"))
		(segment (name "street") (mode "required"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
