# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpUnaryMinus,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "-"
	(e-field-access
		(e-ident (raw "rec1"))
		(e-ident (raw "field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "negate") (constraint-fn-var 13)
	(receiver
		(e-field-access (field "field")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.negate : a -> a]"))
~~~
