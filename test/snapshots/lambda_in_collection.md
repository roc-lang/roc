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
OpenRound(1:1-1:2),
OpBar(2:2-2:3),
LowerIdent(3:3-3:4),Comma(3:4-3:5),
LowerIdent(4:3-4:4),Comma(4:4-4:5),
OpBar(5:2-5:3),OpenCurly(5:4-5:5),
LowerIdent(6:3-6:4),OpPlus(6:5-6:6),LowerIdent(6:7-6:8),
CloseCurly(7:2-7:3),Comma(7:3-7:4),
OpBar(8:2-8:3),LowerIdent(8:3-8:4),Comma(8:4-8:5),LowerIdent(8:6-8:7),OpBar(8:7-8:8),OpenCurly(8:9-8:10),
LowerIdent(9:3-9:4),OpBinaryMinus(9:5-9:6),LowerIdent(9:7-9:8),
CloseCurly(10:2-10:3),Comma(10:3-10:4),
CloseRound(11:1-11:2),EndOfFile(11:2-11:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-11.2
	(e-lambda @2.2-7.3
		(args
			(p-ident @3.3-3.4 (raw "a"))
			(p-ident @4.3-4.4 (raw "b")))
		(e-block @5.4-7.3
			(statements
				(e-binop @6.3-6.8 (op "+")
					(e-ident @6.3-6.4 (raw "a"))
					(e-ident @6.7-6.8 (raw "b"))))))
	(e-lambda @8.2-10.3
		(args
			(p-ident @8.3-8.4 (raw "a"))
			(p-ident @8.6-8.7 (raw "b")))
		(e-block @8.9-10.3
			(statements
				(e-binop @9.3-9.8 (op "-")
					(e-ident @9.3-9.4 (raw "a"))
					(e-ident @9.7-9.8 (raw "b")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-11.2
	(elems
		(e-lambda @2.2-7.3
			(args
				(p-assign @3.3-3.4 (ident "a"))
				(p-assign @4.3-4.4 (ident "b")))
			(e-block @5.4-7.3
				(e-binop @6.3-6.8 (op "add")
					(e-lookup-local @6.3-6.4
						(p-assign @3.3-3.4 (ident "a")))
					(e-lookup-local @6.7-6.8
						(p-assign @4.3-4.4 (ident "b"))))))
		(e-lambda @8.2-10.3
			(args
				(p-assign @8.3-8.4 (ident "a"))
				(p-assign @8.6-8.7 (ident "b")))
			(e-block @8.9-10.3
				(e-binop @9.3-9.8 (op "sub")
					(e-lookup-local @9.3-9.4
						(p-assign @8.3-8.4 (ident "a")))
					(e-lookup-local @9.7-9.8
						(p-assign @8.6-8.7 (ident "b"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-11.2 (type "(Num(_size), Num(_size2) -> Num(_size3), Num(_size4), Num(_size5) -> Num(_size6))"))
~~~
