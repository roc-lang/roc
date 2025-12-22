# META
~~~ini
description=Mono test: identity lambda
type=mono
~~~
# SOURCE
~~~roc
|x| x
~~~
# MONO
~~~roc
|x| x
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-ident (raw "x")))
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a"))
~~~
