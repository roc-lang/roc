# META
~~~ini
description=Test tuple pattern matching
type=expr
~~~
# SOURCE
~~~roc
|(a, b)| a + b
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-tuple
			(p-ident (raw "a"))
			(p-ident (raw "b"))))
	(e-binop (op "+")
		(e-ident (raw "a"))
		(e-ident (raw "b"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-tuple
			(patterns
				(p-assign (ident "a"))
				(p-assign (ident "b")))))
	(e-binop (op "add")
		(e-lookup-local
			(p-assign (ident "a")))
		(e-lookup-local
			(p-assign (ident "b")))))
~~~
# TYPES
~~~clojure
(expr (type "(c, d) -> c where [c.plus : c, d -> c]"))
~~~
