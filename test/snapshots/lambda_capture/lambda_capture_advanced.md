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
(e-call (constraint-fn-var 168)
	(e-call (constraint-fn-var 134)
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b"))
				(p-assign (ident "c")))
			(e-closure
				(captures
					(capture (ident "a"))
					(capture (ident "b"))
					(capture (ident "c")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-dispatch-call (method "plus") (constraint-fn-var 33)
						(receiver
							(e-dispatch-call (method "plus") (constraint-fn-var 31)
								(receiver
									(e-dispatch-call (method "plus") (constraint-fn-var 29)
										(receiver
											(e-lookup-local
												(p-assign (ident "a"))))
										(args
											(e-lookup-local
												(p-assign (ident "b"))))))
								(args
									(e-lookup-local
										(p-assign (ident "c"))))))
						(args
							(e-lookup-local
								(p-assign (ident "x"))))))))
		(e-num (value "10"))
		(e-num (value "20"))
		(e-num (value "5")))
	(e-num (value "7")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
