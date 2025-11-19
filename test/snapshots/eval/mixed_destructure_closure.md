# META
~~~ini
description=Mixed destructue patterns
type=expr
~~~
# SOURCE
~~~roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,CloseRound,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,OpenRound,Int,Comma,Int,CloseRound,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-record
					(field (name "a") (rest false))
					(field (name "x") (rest false)
						(p-tuple
							(p-ident (raw "b"))
							(p-ident (raw "c"))))
					(field (name "y") (rest false)
						(p-record
							(field (name "d") (rest false))
							(field (name "e") (rest false))))))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-binop (op "+")
						(e-binop (op "+")
							(e-ident (raw "a"))
							(e-ident (raw "b")))
						(e-ident (raw "c")))
					(e-ident (raw "d")))
				(e-ident (raw "e")))))
	(e-record
		(field (field "a")
			(e-int (raw "1")))
		(field (field "x")
			(e-tuple
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(field (field "y")
			(e-record
				(field (field "d")
					(e-int (raw "4")))
				(field (field "e")
					(e-int (raw "5")))))))
~~~
# FORMATTED
~~~roc
(|{ a, x: (b, c), y: { d, e } }| a + b + c + d + e)({ a: 1, x: (2, 3), y: { d: 4, e: 5 } })
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lambda
		(args
			(p-record-destructure
				(destructs
					(record-destruct (label "a") (ident "a")
						(required
							(p-assign (ident "a"))))
					(record-destruct (label "x") (ident "x")
						(sub-pattern
							(p-tuple
								(patterns
									(p-assign (ident "b"))
									(p-assign (ident "c"))))))
					(record-destruct (label "y") (ident "y")
						(sub-pattern
							(p-record-destructure
								(destructs
									(record-destruct (label "d") (ident "d")
										(required
											(p-assign (ident "d"))))
									(record-destruct (label "e") (ident "e")
										(required
											(p-assign (ident "e")))))))))))
		(e-binop (op "add")
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
					(p-assign (ident "d"))))
			(e-lookup-local
				(p-assign (ident "e")))))
	(e-record
		(fields
			(field (name "a")
				(e-num (value "1")))
			(field (name "x")
				(e-tuple
					(elems
						(e-num (value "2"))
						(e-num (value "3")))))
			(field (name "y")
				(e-record
					(fields
						(field (name "d")
							(e-num (value "4")))
						(field (name "e")
							(e-num (value "5")))))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
