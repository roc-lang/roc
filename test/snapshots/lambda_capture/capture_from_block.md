# META
~~~ini
description="A lambda within a block expression captures a variable also defined within that block."
type=expr
~~~
# SOURCE
~~~roc
{
    a = 10
    b = (|_| a * 2)(5)
    b
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:11),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),OpenRound(3:9-3:10),OpBar(3:10-3:11),Underscore(3:11-3:12),OpBar(3:12-3:13),LowerIdent(3:14-3:15),OpStar(3:16-3:17),Int(3:18-3:19),CloseRound(3:19-3:20),NoSpaceOpenRound(3:20-3:21),Int(3:21-3:22),CloseRound(3:22-3:23),
LowerIdent(4:5-4:6),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.5-2.11
			(p-ident @2.5-2.6 (raw "a"))
			(e-int @2.9-2.11 (raw "10")))
		(s-decl @3.5-3.23
			(p-ident @3.5-3.6 (raw "b"))
			(e-apply @3.9-3.23
				(e-tuple @3.9-3.20
					(e-lambda @3.10-3.19
						(args
							(p-underscore))
						(e-binop @3.14-3.19 (op "*")
							(e-ident @3.14-3.15 (raw "a"))
							(e-int @3.18-3.19 (raw "2")))))
				(e-int @3.21-3.22 (raw "5"))))
		(e-ident @4.5-4.6 (raw "b"))))
~~~
# FORMATTED
~~~roc
{
	a = 10
	b = (|_| a * 2)(5)
	b
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.11
		(p-assign @2.5-2.6 (ident "a"))
		(e-int @2.9-2.11 (value "10")))
	(s-let @3.5-3.23
		(p-assign @3.5-3.6 (ident "b"))
		(e-call @3.9-3.23
			(e-closure @3.10-3.19
				(captures
					(capture @2.5-2.6 (ident "a")))
				(e-lambda @3.10-3.19
					(args
						(p-underscore @3.11-3.12))
					(e-binop @3.14-3.19 (op "mul")
						(e-lookup-local @3.14-3.15
							(p-assign @2.5-2.6 (ident "a")))
						(e-int @3.18-3.19 (value "2")))))
			(e-int @3.21-3.22 (value "5"))))
	(e-lookup-local @4.5-4.6
		(p-assign @3.5-3.6 (ident "b"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
