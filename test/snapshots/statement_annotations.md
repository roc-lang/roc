# META
~~~ini
description=Inline annotation for statements
type=snippet
~~~
# SOURCE
~~~roc
scopedTypeVarInternal : val -> val
scopedTypeVarInternal = |a| {
	b : other_val -> other_val
	b = |c| {
		d : other_val
		d = c

		d
	}

	b(a)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:22),OpColon(1:23-1:24),LowerIdent(1:25-1:28),OpArrow(1:29-1:31),LowerIdent(1:32-1:35),
LowerIdent(2:1-2:22),OpAssign(2:23-2:24),OpBar(2:25-2:26),LowerIdent(2:26-2:27),OpBar(2:27-2:28),OpenCurly(2:29-2:30),
LowerIdent(3:2-3:3),OpColon(3:4-3:5),LowerIdent(3:6-3:15),OpArrow(3:16-3:18),LowerIdent(3:19-3:28),
LowerIdent(4:2-4:3),OpAssign(4:4-4:5),OpBar(4:6-4:7),LowerIdent(4:7-4:8),OpBar(4:8-4:9),OpenCurly(4:10-4:11),
LowerIdent(5:3-5:4),OpColon(5:5-5:6),LowerIdent(5:7-5:16),
LowerIdent(6:3-6:4),OpAssign(6:5-6:6),LowerIdent(6:7-6:8),
LowerIdent(8:3-8:4),
CloseCurly(9:2-9:3),
LowerIdent(11:2-11:3),NoSpaceOpenRound(11:3-11:4),LowerIdent(11:4-11:5),CloseRound(11:5-11:6),
CloseCurly(12:1-12:2),
EndOfFile(13:1-13:1),
~~~
# PARSE
~~~clojure
(file @1.1-12.2
	(type-module @1.1-1.22)
	(statements
		(s-type-anno @1.1-1.35 (name "scopedTypeVarInternal")
			(ty-fn @1.25-1.35
				(ty-var @1.25-1.28 (raw "val"))
				(ty-var @1.32-1.35 (raw "val"))))
		(s-decl @2.1-12.2
			(p-ident @2.1-2.22 (raw "scopedTypeVarInternal"))
			(e-lambda @2.25-12.2
				(args
					(p-ident @2.26-2.27 (raw "a")))
				(e-block @2.29-12.2
					(statements
						(s-type-anno @3.2-3.28 (name "b")
							(ty-fn @3.6-3.28
								(ty-var @3.6-3.15 (raw "other_val"))
								(ty-var @3.19-3.28 (raw "other_val"))))
						(s-decl @4.2-9.3
							(p-ident @4.2-4.3 (raw "b"))
							(e-lambda @4.6-9.3
								(args
									(p-ident @4.7-4.8 (raw "c")))
								(e-block @4.10-9.3
									(statements
										(s-type-anno @5.3-5.16 (name "d")
											(ty-var @5.7-5.16 (raw "other_val")))
										(s-decl @6.3-6.8
											(p-ident @6.3-6.4 (raw "d"))
											(e-ident @6.7-6.8 (raw "c")))
										(e-ident @8.3-8.4 (raw "d"))))))
						(e-apply @11.2-11.6
							(e-ident @11.2-11.3 (raw "b"))
							(e-ident @11.4-11.5 (raw "a")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.22 (ident "scopedTypeVarInternal"))
		(e-lambda @2.25-12.2
			(args
				(p-assign @2.26-2.27 (ident "a")))
			(e-block @2.29-12.2
				(s-let @4.2-9.3
					(p-assign @4.2-4.3 (ident "b"))
					(e-lambda @4.6-9.3
						(args
							(p-assign @4.7-4.8 (ident "c")))
						(e-block @4.10-9.3
							(s-let @6.3-6.8
								(p-assign @6.3-6.4 (ident "d"))
								(e-lookup-local @6.7-6.8
									(p-assign @4.7-4.8 (ident "c"))))
							(e-lookup-local @8.3-8.4
								(p-assign @6.3-6.4 (ident "d"))))))
				(e-call @11.2-11.6
					(e-lookup-local @11.2-11.3
						(p-assign @4.2-4.3 (ident "b")))
					(e-lookup-local @11.4-11.5
						(p-assign @2.26-2.27 (ident "a"))))))
		(annotation @2.1-2.22
			(declared-type
				(ty-fn @1.25-1.35 (effectful false)
					(ty-rigid-var @1.25-1.28 (name "val"))
					(ty-rigid-var-lookup (ty-rigid-var @1.25-1.28 (name "val"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.22 (type "val -> val")))
	(expressions
		(expr @2.25-12.2 (type "val -> val"))))
~~~
