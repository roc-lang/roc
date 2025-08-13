# META
~~~ini
description="The test case from the original request, involving multiple levels of nesting and local assignments. This will be the ultimate validation."
type=expr
~~~
# SOURCE
~~~roc
(((|a| {
    a_loc = a * 2
    |b| {
        b_loc = a_loc + b
        |c| b_loc + c
    }
})(100))(20))(3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),OpBar(1:4-1:5),LowerIdent(1:5-1:6),OpBar(1:6-1:7),OpenCurly(1:8-1:9),
LowerIdent(2:5-2:10),OpAssign(2:11-2:12),LowerIdent(2:13-2:14),OpStar(2:15-2:16),Int(2:17-2:18),
OpBar(3:5-3:6),LowerIdent(3:6-3:7),OpBar(3:7-3:8),OpenCurly(3:9-3:10),
LowerIdent(4:9-4:14),OpAssign(4:15-4:16),LowerIdent(4:17-4:22),OpPlus(4:23-4:24),LowerIdent(4:25-4:26),
OpBar(5:9-5:10),LowerIdent(5:10-5:11),OpBar(5:11-5:12),LowerIdent(5:13-5:18),OpPlus(5:19-5:20),LowerIdent(5:21-5:22),
CloseCurly(6:5-6:6),
CloseCurly(7:1-7:2),CloseRound(7:2-7:3),NoSpaceOpenRound(7:3-7:4),Int(7:4-7:7),CloseRound(7:7-7:8),CloseRound(7:8-7:9),NoSpaceOpenRound(7:9-7:10),Int(7:10-7:12),CloseRound(7:12-7:13),CloseRound(7:13-7:14),NoSpaceOpenRound(7:14-7:15),Int(7:15-7:16),CloseRound(7:16-7:17),EndOfFile(7:17-7:17),
~~~
# PARSE
~~~clojure
(e-apply @1.1-7.17
	(e-tuple @1.1-7.14
		(e-apply @1.2-7.13
			(e-tuple @1.2-7.9
				(e-apply @1.3-7.8
					(e-tuple @1.3-7.3
						(e-lambda @1.4-7.2
							(args
								(p-ident @1.5-1.6 (raw "a")))
							(e-block @1.8-7.2
								(statements
									(s-decl @2.5-2.18
										(p-ident @2.5-2.10 (raw "a_loc"))
										(e-binop @2.13-2.18 (op "*")
											(e-ident @2.13-2.14 (raw "a"))
											(e-int @2.17-2.18 (raw "2"))))
									(e-lambda @3.5-6.6
										(args
											(p-ident @3.6-3.7 (raw "b")))
										(e-block @3.9-6.6
											(statements
												(s-decl @4.9-4.26
													(p-ident @4.9-4.14 (raw "b_loc"))
													(e-binop @4.17-4.26 (op "+")
														(e-ident @4.17-4.22 (raw "a_loc"))
														(e-ident @4.25-4.26 (raw "b"))))
												(e-lambda @5.9-5.22
													(args
														(p-ident @5.10-5.11 (raw "c")))
													(e-binop @5.13-5.22 (op "+")
														(e-ident @5.13-5.18 (raw "b_loc"))
														(e-ident @5.21-5.22 (raw "c")))))))))))
					(e-int @7.4-7.7 (raw "100"))))
			(e-int @7.10-7.12 (raw "20"))))
	(e-int @7.15-7.16 (raw "3")))
~~~
# FORMATTED
~~~roc
(
	(
		(
			|a| {
				a_loc = a * 2
				|b| {
					b_loc = a_loc + b
					|c| b_loc + c
				}
			},
		)(100),
	)(20),
)(3)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-7.17
	(e-call @1.2-7.13
		(e-call @1.3-7.8
			(e-lambda @1.4-7.2
				(args
					(p-assign @1.5-1.6 (ident "a")))
				(e-block @1.8-7.2
					(s-let @2.5-2.18
						(p-assign @2.5-2.10 (ident "a_loc"))
						(e-binop @2.13-2.18 (op "mul")
							(e-lookup-local @2.13-2.14
								(p-assign @1.5-1.6 (ident "a")))
							(e-int @2.17-2.18 (value "2"))))
					(e-closure @3.5-6.6
						(captures
							(capture @2.5-2.10 (ident "a_loc")))
						(e-lambda @3.5-6.6
							(args
								(p-assign @3.6-3.7 (ident "b")))
							(e-block @3.9-6.6
								(s-let @4.9-4.26
									(p-assign @4.9-4.14 (ident "b_loc"))
									(e-binop @4.17-4.26 (op "add")
										(e-lookup-local @4.17-4.22
											(p-assign @2.5-2.10 (ident "a_loc")))
										(e-lookup-local @4.25-4.26
											(p-assign @3.6-3.7 (ident "b")))))
								(e-closure @5.9-5.22
									(captures
										(capture @4.9-4.14 (ident "b_loc")))
									(e-lambda @5.9-5.22
										(args
											(p-assign @5.10-5.11 (ident "c")))
										(e-binop @5.13-5.22 (op "add")
											(e-lookup-local @5.13-5.18
												(p-assign @4.9-4.14 (ident "b_loc")))
											(e-lookup-local @5.21-5.22
												(p-assign @5.10-5.11 (ident "c")))))))))))
			(e-int @7.4-7.7 (value "100")))
		(e-int @7.10-7.12 (value "20")))
	(e-int @7.15-7.16 (value "3")))
~~~
# TYPES
~~~clojure
(expr @1.1-7.17 (type "Num(_size)"))
~~~
