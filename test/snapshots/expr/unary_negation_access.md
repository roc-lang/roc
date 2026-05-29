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
(e-unary-minus
	(e-dot-access (field "field")
		(receiver
			(e-lookup-local
				(p-assign (ident "rec1"))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.negate : a -> a]"))
~~~
