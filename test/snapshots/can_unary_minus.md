# META
~~~ini
description=Test unary minus operator
type=expr
~~~
# SOURCE
~~~roc
|x| -x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpUnaryMinus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(unary "-"
		(e-ident (raw "x"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-unary-minus
		(e-lookup-local
			(p-assign (ident "x")))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.negate : a -> a]"))
~~~
