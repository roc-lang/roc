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
		(receiver
			(e-ident (raw "rec1")))
		(segment (mode "required") (field "field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-minus
	(e-field-access
		(receiver
			(e-runtime-error (tag "ident_not_in_scope")))
		(segments
			(segment (name "field") (mode "required")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
