# META
~~~ini
description=Tuple as an argument
type=expr
~~~
# SOURCE
~~~roc
(|(x,y)| x * y )((1,2))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,LowerIdent,OpStar,LowerIdent,CloseRound,NoSpaceOpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-tuple
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-ident (raw "y")))))
	(e-tuple
		(e-int (raw "1"))
		(e-int (raw "2"))))
~~~
# FORMATTED
~~~roc
(|(x, y)| x * y)((1, 2))
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lambda
		(args
			(p-tuple
				(patterns
					(p-assign (ident "x"))
					(p-assign (ident "y")))))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "y")))))
	(e-tuple
		(elems
			(e-num (value "1"))
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
