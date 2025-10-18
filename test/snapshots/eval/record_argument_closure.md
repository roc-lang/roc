# META
~~~ini
description=Record as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,LowerIdent,OpStar,LowerIdent,CloseRound,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-record
					(field (name "x") (rest false))
					(field (name "y") (rest false))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-ident (raw "y")))))
	(e-record
		(field (field "x")
			(e-int (raw "10")))
		(field (field "y")
			(e-int (raw "20")))))
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
			(p-record-destructure
				(destructs
					(record-destruct (label "x") (ident "x")
						(required
							(p-assign (ident "x"))))
					(record-destruct (label "y") (ident "y")
						(required
							(p-assign (ident "y")))))))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "y")))))
	(e-record
		(fields
			(field (name "x")
				(e-num (value "10")))
			(field (name "y")
				(e-num (value "20"))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
