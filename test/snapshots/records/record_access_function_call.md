# META
~~~ini
description=Record field access with function call
type=expr
~~~
# SOURCE
~~~roc
(person.transform)(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-field-access
			(e-ident (raw "person"))
			(e-ident (raw "transform"))))
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 46)
	(e-field-access (field "transform")
		(receiver
			(e-lookup-local
				(p-assign (ident "person")))))
	(e-num (value "42")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
