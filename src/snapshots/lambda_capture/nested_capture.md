# META
~~~ini
description="An inner lambda captures a variable defined in an outer lambda's scope."
type=expr
~~~
# SOURCE
~~~roc
{
    f = (|a| |b| a + b)
    g = f(10)
    g(5) # Expect: 15
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),OpenRound(2:9-2:10),OpBar(2:10-2:11),LowerIdent(2:11-2:12),OpBar(2:12-2:13),OpBar(2:14-2:15),LowerIdent(2:15-2:16),OpBar(2:16-2:17),LowerIdent(2:18-2:19),OpPlus(2:20-2:21),LowerIdent(2:22-2:23),CloseRound(2:23-2:24),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),LowerIdent(3:9-3:10),NoSpaceOpenRound(3:10-3:11),Int(3:11-3:13),CloseRound(3:13-3:14),
LowerIdent(4:5-4:6),NoSpaceOpenRound(4:6-4:7),Int(4:7-4:8),CloseRound(4:8-4:9),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.5-2.24
			(p-ident @2.5-2.6 (raw "f"))
			(e-tuple @2.9-2.24
				(e-lambda @2.10-2.23
					(args
						(p-ident @2.11-2.12 (raw "a")))
					(e-lambda @2.14-2.23
						(args
							(p-ident @2.15-2.16 (raw "b")))
						(e-binop @2.18-2.23 (op "+")
							(e-ident @2.18-2.19 (raw "a"))
							(e-ident @2.22-2.23 (raw "b")))))))
		(s-decl @3.5-3.14
			(p-ident @3.5-3.6 (raw "g"))
			(e-apply @3.9-3.14
				(e-ident @3.9-3.10 (raw "f"))
				(e-int @3.11-3.13 (raw "10"))))
		(e-apply @4.5-4.9
			(e-ident @4.5-4.6 (raw "g"))
			(e-int @4.7-4.8 (raw "5")))))
~~~
# FORMATTED
~~~roc
{
	f = (|a| |b| a + b)
	g = f(10)
	g(5) # Expect: 15
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.24
		(p-assign @2.5-2.6 (ident "f"))
		(e-lambda @2.10-2.23
			(args
				(p-assign @2.11-2.12 (ident "a")))
			(e-closure @2.14-2.23
				(captures
					(capture @2.11-2.12 (ident "a")))
				(e-lambda @2.14-2.23
					(args
						(p-assign @2.15-2.16 (ident "b")))
					(e-binop @2.18-2.23 (op "add")
						(e-lookup-local @2.18-2.19
							(p-assign @2.11-2.12 (ident "a")))
						(e-lookup-local @2.22-2.23
							(p-assign @2.15-2.16 (ident "b"))))))))
	(s-let @3.5-3.14
		(p-assign @3.5-3.6 (ident "g"))
		(e-call @3.9-3.14
			(e-lookup-local @3.9-3.10
				(p-assign @2.5-2.6 (ident "f")))
			(e-int @3.11-3.13 (value "10"))))
	(e-call @4.5-4.9
		(e-lookup-local @4.5-4.6
			(p-assign @3.5-3.6 (ident "g")))
		(e-int @4.7-4.8 (value "5"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
