# META
~~~ini
description=Dot access with proper variable definitions
type=expr
~~~
# SOURCE
~~~roc
{
    list = [1, 2, 3]
    fn = |x| x + 1
    list.map(fn)
}
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:9),OpAssign(2:10-2:11),OpenSquare(2:12-2:13),Int(2:13-2:14),Comma(2:14-2:15),Int(2:16-2:17),Comma(2:17-2:18),Int(2:19-2:20),CloseSquare(2:20-2:21),Newline(1:1-1:1),
LowerIdent(3:5-3:7),OpAssign(3:8-3:9),OpBar(3:10-3:11),LowerIdent(3:11-3:12),OpBar(3:12-3:13),LowerIdent(3:14-3:15),OpPlus(3:16-3:17),Int(3:18-3:19),Newline(1:1-1:1),
LowerIdent(4:5-4:9),NoSpaceDotLowerIdent(4:9-4:13),NoSpaceOpenRound(4:13-4:14),LowerIdent(4:14-4:16),CloseRound(4:16-4:17),Newline(1:1-1:1),
CloseCurly(5:1-5:2),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.5-2.21
			(p-ident @2.5-2.9 (raw "list"))
			(e-list @2.12-2.21
				(e-int @2.13-2.14 (raw "1"))
				(e-int @2.16-2.17 (raw "2"))
				(e-int @2.19-2.20 (raw "3"))))
		(s-decl @3.5-4.9
			(p-ident @3.5-3.7 (raw "fn"))
			(e-lambda @3.10-4.9
				(args
					(p-ident @3.11-3.12 (raw "x")))
				(e-binop @3.14-4.9 (op "+")
					(e-ident @3.14-3.15 (raw "x"))
					(e-int @3.18-3.19 (raw "1")))))
		(e-field-access @4.5-5.2
			(e-ident @4.5-4.9 (raw "list"))
			(e-apply @4.9-4.17
				(e-ident @4.9-4.13 (raw "map"))
				(e-ident @4.14-4.16 (raw "fn"))))))
~~~
# FORMATTED
~~~roc
{
	list = [1, 2, 3]
	fn = |x| x + 1
	list.map(fn)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.21
		(p-assign @2.5-2.9 (ident "list"))
		(e-list @2.12-2.21
			(elems
				(e-int @2.13-2.14 (value "1"))
				(e-int @2.16-2.17 (value "2"))
				(e-int @2.19-2.20 (value "3")))))
	(s-let @3.5-4.9
		(p-assign @3.5-3.7 (ident "fn"))
		(e-lambda @3.10-4.9
			(args
				(p-assign @3.11-3.12 (ident "x")))
			(e-binop @3.14-4.9 (op "add")
				(e-lookup-local @3.14-3.15
					(pattern @3.11-3.12))
				(e-int @3.18-3.19 (value "1")))))
	(e-dot-access @4.5-5.2 (field "map")
		(receiver
			(e-lookup-local @4.5-4.9
				(pattern @2.5-2.9)))
		(args
			(e-lookup-local @4.14-4.16
				(pattern @3.5-3.7)))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
