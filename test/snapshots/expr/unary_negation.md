# META
~~~ini
description=unary_negation
type=expr
~~~
# SOURCE
~~~roc
-foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpUnaryMinus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "-"
	(e-ident (raw "foo")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "negate") (constraint-fn-var 9)
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
