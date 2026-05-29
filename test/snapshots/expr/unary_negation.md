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
UNDEFINED VARIABLE - unary_negation.md:1:2:1:5
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
(e-unary-minus
	(e-lookup-local
		(p-assign (ident "foo"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.negate : a -> a]"))
~~~
