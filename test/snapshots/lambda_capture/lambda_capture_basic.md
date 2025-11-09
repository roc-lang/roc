# META
~~~ini
description=Basic lambda capture detection during canonicalization
type=expr
~~~
# SOURCE
~~~roc
(|x| |y| x + y)(1)(2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-apply
		(e-tuple
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-lambda
					(args
						(p-ident (raw "y")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))
		(e-int (raw "1")))
	(e-int (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-call
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-closure
				(captures
					(capture (ident "x")))
				(e-lambda
					(args
						(p-assign (ident "y")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))
		(e-num (value "1")))
	(e-num (value "2")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
