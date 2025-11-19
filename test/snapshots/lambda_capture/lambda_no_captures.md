# META
~~~ini
description=Lambda with no captures
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "x")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1")))))
	(e-int (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "x")))
			(e-num (value "1"))))
	(e-num (value "2")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
