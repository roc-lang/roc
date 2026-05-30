# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-method-call (method ".map")
	(receiver
		(e-ident (raw "list")))
	(args
		(e-ident (raw "fn"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "map") (constraint-fn-var 11)
	(receiver
		(e-lookup-local
			(p-assign (ident "list"))))
	(args
		(e-lookup-local
			(p-assign (ident "fn")))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
