# META
~~~ini
description=Lambda inside a collection
type=expr
~~~
# SOURCE
~~~roc
(
	|
		a,
		b,
	| {
		a + b
	},
	|a, b| {
		a - b
	},
)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,
OpBar,
LowerIdent,Comma,
LowerIdent,Comma,
OpBar,OpenCurly,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,Comma,
OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpBinaryMinus,LowerIdent,
CloseCurly,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-lambda
		(args
			(p-ident (raw "a"))
			(p-ident (raw "b")))
		(e-block
			(statements
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "b"))))))
	(e-lambda
		(args
			(p-ident (raw "a"))
			(p-ident (raw "b")))
		(e-block
			(statements
				(e-binop (op "-")
					(e-ident (raw "a"))
					(e-ident (raw "b")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-block
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "a")))
					(e-lookup-local
						(p-assign (ident "b"))))))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-block
				(e-binop (op "sub")
					(e-lookup-local
						(p-assign (ident "a")))
					(e-lookup-local
						(p-assign (ident "b"))))))))
~~~
# TYPES
~~~clojure
(expr (type "(Num(_size), Num(_size2) -> Num(_size3), Num(_size4), Num(_size5) -> Num(_size6))"))
~~~
