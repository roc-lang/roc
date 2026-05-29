# META
~~~ini
description=unary_not
type=expr
~~~
# SOURCE
~~~roc
!blah
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_not.md:1:2:1:6
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
	(e-ident (raw "blah")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not
	(e-lookup-local
		(p-assign (ident "blah"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.not : a -> a]"))
~~~
