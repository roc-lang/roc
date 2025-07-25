# META
~~~ini
description=Mixed capture patterns in block expression
type=expr
~~~
# SOURCE
~~~roc
(|base| {
		simple = |x| base + x + 1
		simple(1)
})(1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:7),OpBar(1:7-1:8),OpenCurly(1:9-1:10),
LowerIdent(2:3-2:9),OpAssign(2:10-2:11),OpBar(2:12-2:13),LowerIdent(2:13-2:14),OpBar(2:14-2:15),LowerIdent(2:16-2:20),OpPlus(2:21-2:22),LowerIdent(2:23-2:24),OpPlus(2:25-2:26),Int(2:27-2:28),
LowerIdent(3:3-3:9),NoSpaceOpenRound(3:9-3:10),Int(3:10-3:11),CloseRound(3:11-3:12),
CloseCurly(4:1-4:2),CloseRound(4:2-4:3),NoSpaceOpenRound(4:3-4:4),Int(4:4-4:5),CloseRound(4:5-4:6),EndOfFile(4:6-4:6),
~~~
# PARSE
~~~clojure
(e-apply @1.1-4.6
	(e-tuple @1.1-4.3
		(e-lambda @1.2-4.2
			(args
				(p-ident @1.3-1.7 (raw "base")))
			(e-block @1.9-4.2
				(statements
					(s-decl @2.3-2.28
						(p-ident @2.3-2.9 (raw "simple"))
						(e-lambda @2.12-2.28
							(args
								(p-ident @2.13-2.14 (raw "x")))
							(e-binop @2.16-2.28 (op "+")
								(e-ident @2.16-2.20 (raw "base"))
								(e-binop @2.23-2.28 (op "+")
									(e-ident @2.23-2.24 (raw "x"))
									(e-int @2.27-2.28 (raw "1"))))))
					(e-apply @3.3-3.12
						(e-ident @3.3-3.9 (raw "simple"))
						(e-int @3.10-3.11 (raw "1")))))))
	(e-int @4.4-4.5 (raw "1")))
~~~
# FORMATTED
~~~roc
(
	|base| {
		simple = |x| base + x + 1
		simple(1)
	},
)(1)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-4.6
	(e-lambda @1.2-4.2
		(args
			(p-assign @1.3-1.7 (ident "base")))
		(e-block @1.9-4.2
			(s-let @2.3-2.28
				(p-assign @2.3-2.9 (ident "simple"))
				(e-closure @2.12-2.28
					(captures
						(capture @1.3-1.7 (ident "base")))
					(e-lambda @2.12-2.28
						(args
							(p-assign @2.13-2.14 (ident "x")))
						(e-binop @2.16-2.28 (op "add")
							(e-lookup-local @2.16-2.20
								(p-assign @1.3-1.7 (ident "base")))
							(e-binop @2.23-2.28 (op "add")
								(e-lookup-local @2.23-2.24
									(p-assign @2.13-2.14 (ident "x")))
								(e-int @2.27-2.28 (value "1")))))))
			(e-call @3.3-3.12
				(e-lookup-local @3.3-3.9
					(p-assign @2.3-2.9 (ident "simple")))
				(e-int @3.10-3.11 (value "1")))))
	(e-int @4.4-4.5 (value "1")))
~~~
# TYPES
~~~clojure
(expr @1.1-4.6 (type "Num(_size)"))
~~~
