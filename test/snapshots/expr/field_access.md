# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(receiver
		(e-ident (raw "person")))
	(segment (mode "required") (field "name")))
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
		(segment (name "name") (mode "required"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
