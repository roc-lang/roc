# META
~~~ini
description=Mixed capture patterns in block expression - some lambdas capture, others don't
type=expr
~~~
# SOURCE
~~~roc
|base| {
    simple = |x| base + x + 1
    simple(1)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar(1:1-1:2),LowerIdent(1:2-1:6),OpBar(1:6-1:7),OpenCurly(1:8-1:9),
LowerIdent(2:5-2:11),OpAssign(2:12-2:13),OpBar(2:14-2:15),LowerIdent(2:15-2:16),OpBar(2:16-2:17),LowerIdent(2:18-2:22),OpPlus(2:23-2:24),LowerIdent(2:25-2:26),OpPlus(2:27-2:28),Int(2:29-2:30),
LowerIdent(3:5-3:11),NoSpaceOpenRound(3:11-3:12),Int(3:12-3:13),CloseRound(3:13-3:14),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-4.2
	(args
		(p-ident @1.2-1.6 (raw "base")))
	(e-block @1.8-4.2
		(statements
			(s-decl @2.5-2.30
				(p-ident @2.5-2.11 (raw "simple"))
				(e-lambda @2.14-2.30
					(args
						(p-ident @2.15-2.16 (raw "x")))
					(e-binop @2.18-2.30 (op "+")
						(e-ident @2.18-2.22 (raw "base"))
						(e-binop @2.25-2.30 (op "+")
							(e-ident @2.25-2.26 (raw "x"))
							(e-int @2.29-2.30 (raw "1"))))))
			(e-apply @3.5-3.14
				(e-ident @3.5-3.11 (raw "simple"))
				(e-int @3.12-3.13 (raw "1"))))))
~~~
# FORMATTED
~~~roc
|base| {
	simple = |x| base + x + 1
	simple(1)
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-4.2
	(args
		(p-assign @1.2-1.6 (ident "base")))
	(e-block @1.8-4.2
		(s-let @2.5-2.30
			(p-assign @2.5-2.11 (ident "simple"))
			(e-lambda @2.14-2.30
				(args
					(p-assign @2.15-2.16 (ident "x")))
				(captures
					(capture (name "base")))
				(e-binop @2.18-2.30 (op "add")
					(e-lookup-local @2.18-2.22
						(p-assign @1.2-1.6 (ident "base")))
					(e-binop @2.25-2.30 (op "add")
						(e-lookup-local @2.25-2.26
							(p-assign @2.15-2.16 (ident "x")))
						(e-int @2.29-2.30 (value "1"))))))
		(e-call @3.5-3.14
			(e-lookup-local @3.5-3.11
				(p-assign @2.5-2.11 (ident "simple")))
			(e-int @3.12-3.13 (value "1")))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "Num(_size) -> Num(_size2)"))
~~~
