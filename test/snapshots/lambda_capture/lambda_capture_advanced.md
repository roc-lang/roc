# META
~~~ini
description=More davanced lambda capture
type=expr
~~~
# SOURCE
~~~roc
(|a, b, c| |x| a + b + c + x)(10, 20, 5)(7)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,CloseRound,NoSpaceOpenRound,Int,Comma,Int,Comma,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-apply
		(e-tuple
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b"))
					(p-ident (raw "c")))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-binop (op "+")
							(e-binop (op "+")
								(e-ident (raw "a"))
								(e-ident (raw "b")))
							(e-ident (raw "c")))
						(e-ident (raw "x"))))))
		(e-int (raw "10"))
		(e-int (raw "20"))
		(e-int (raw "5")))
	(e-int (raw "7")))
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
				(p-assign (ident "a"))
				(p-assign (ident "b"))
				(p-assign (ident "c")))
			(e-closure
				(captures
					(capture (ident "c"))
					(capture (ident "a"))
					(capture (ident "b")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "a")))
								(e-lookup-local
									(p-assign (ident "b"))))
							(e-lookup-local
								(p-assign (ident "c"))))
						(e-lookup-local
							(p-assign (ident "x")))))))
		(e-num (value "10"))
		(e-num (value "20"))
		(e-num (value "5")))
	(e-num (value "7")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
