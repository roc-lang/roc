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
			(receiver
				(e-ident (raw "person")))
			(segment (mode "required") (field "transform"))))
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 217)
	(e-field-access
		(receiver
			(e-runtime-error (tag "ident_not_in_scope")))
		(segments
			(segment (name "transform") (mode "required"))))
	(e-num (value "42")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
