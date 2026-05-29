# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_op_not.md:1:2:1:9
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBang,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-ident (raw "isValid")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not
	(e-lookup-local
		(p-assign (ident "isValid"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.not : a -> a]"))
~~~
