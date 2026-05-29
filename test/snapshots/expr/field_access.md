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
	(e-ident (raw "person"))
	(e-ident (raw "name")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-field-access (field "name")
	(receiver
		(e-lookup-local
			(p-assign (ident "person")))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
