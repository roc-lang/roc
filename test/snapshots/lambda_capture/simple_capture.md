# META
~~~ini
description="A basic case where a lambda captures one variable from its immediate parent scope."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| x)(1)
    y
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:10),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),OpenRound(3:9-3:10),OpBar(3:10-3:11),Underscore(3:11-3:12),OpBar(3:12-3:13),LowerIdent(3:14-3:15),CloseRound(3:15-3:16),NoSpaceOpenRound(3:16-3:17),Int(3:17-3:18),CloseRound(3:18-3:19),
LowerIdent(4:5-4:6),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.5-2.10
			(p-ident @2.5-2.6 (raw "x"))
			(e-int @2.9-2.10 (raw "5")))
		(s-decl @3.5-3.19
			(p-ident @3.5-3.6 (raw "y"))
			(e-apply @3.9-3.19
				(e-tuple @3.9-3.16
					(e-lambda @3.10-3.15
						(args
							(p-underscore))
						(e-ident @3.14-3.15 (raw "x"))))
				(e-int @3.17-3.18 (raw "1"))))
		(e-ident @4.5-4.6 (raw "y"))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	y = (|_| x)(1)
	y
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.10
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.10 (value "5")))
	(s-let @3.5-3.19
		(p-assign @3.5-3.6 (ident "y"))
		(e-call @3.9-3.19
			(e-closure @3.10-3.15
				(captures
					(capture @2.5-2.6 (ident "x")))
				(e-lambda @3.10-3.15
					(args
						(p-underscore @3.11-3.12))
					(e-lookup-local @3.14-3.15
						(p-assign @2.5-2.6 (ident "x")))))
			(e-int @3.17-3.18 (value "1"))))
	(e-lookup-local @4.5-4.6
		(p-assign @3.5-3.6 (ident "y"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
