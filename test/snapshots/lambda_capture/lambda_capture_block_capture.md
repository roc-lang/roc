# META
~~~ini
description=Block expression with lambda capture
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    f = |y| x + y
    f(10)
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
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),OpBar(3:9-3:10),LowerIdent(3:10-3:11),OpBar(3:11-3:12),LowerIdent(3:13-3:14),OpPlus(3:15-3:16),LowerIdent(3:17-3:18),
LowerIdent(4:5-4:6),NoSpaceOpenRound(4:6-4:7),Int(4:7-4:9),CloseRound(4:9-4:10),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.5-2.11
			(p-ident @2.5-2.6 (raw "x"))
			(e-int @2.9-2.11 (raw "42")))
		(s-decl @3.5-3.18
			(p-ident @3.5-3.6 (raw "f"))
			(e-lambda @3.9-3.18
				(args
					(p-ident @3.10-3.11 (raw "y")))
				(e-binop @3.13-3.18 (op "+")
					(e-ident @3.13-3.14 (raw "x"))
					(e-ident @3.17-3.18 (raw "y")))))
		(e-apply @4.5-4.10
			(e-ident @4.5-4.6 (raw "f"))
			(e-int @4.7-4.9 (raw "10")))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	f = |y| x + y
	f(10)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.11
		(p-assign @2.5-2.6 (ident "x"))
		(e-num @2.9-2.11 (value "42")))
	(s-let @3.5-3.18
		(p-assign @3.5-3.6 (ident "f"))
		(e-closure @3.9-3.18
			(captures
				(capture @2.5-2.6 (ident "x")))
			(e-lambda @3.9-3.18
				(args
					(p-assign @3.10-3.11 (ident "y")))
				(e-binop @3.13-3.18 (op "add")
					(e-lookup-local @3.13-3.14
						(p-assign @2.5-2.6 (ident "x")))
					(e-lookup-local @3.17-3.18
						(p-assign @3.10-3.11 (ident "y")))))))
	(e-call @4.5-4.10
		(e-num @4.7-4.9 (value "10"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
