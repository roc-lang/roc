# META
~~~ini
description=Test unary not operator
type=expr
~~~
# SOURCE
~~~roc
|x| !x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpBang,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(unary "!"
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
	(e-unary-not
		(e-lookup-local
			(p-assign (ident "x")))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.not : a -> a]"))
~~~
