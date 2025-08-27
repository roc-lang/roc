# META
~~~ini
description=Inline annotation for statements
type=file
~~~
# SOURCE
~~~roc
module []

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
TYPE MISMATCH - statement_annotations.md:13:7:13:8
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:22),OpColon(3:23-3:24),LowerIdent(3:25-3:28),OpArrow(3:29-3:31),LowerIdent(3:32-3:35),
LowerIdent(4:1-4:22),OpAssign(4:23-4:24),OpBar(4:25-4:26),LowerIdent(4:26-4:27),OpBar(4:27-4:28),OpenCurly(4:29-4:30),
LowerIdent(5:2-5:3),OpColon(5:4-5:5),LowerIdent(5:6-5:15),OpArrow(5:16-5:18),LowerIdent(5:19-5:28),
LowerIdent(6:2-6:3),OpAssign(6:4-6:5),OpBar(6:6-6:7),LowerIdent(6:7-6:8),OpBar(6:8-6:9),OpenCurly(6:10-6:11),
LowerIdent(7:3-7:4),OpColon(7:5-7:6),LowerIdent(7:7-7:16),
LowerIdent(8:3-8:4),OpAssign(8:5-8:6),LowerIdent(8:7-8:8),
LowerIdent(10:3-10:4),
CloseCurly(11:2-11:3),
LowerIdent(13:2-13:3),NoSpaceOpenRound(13:3-13:4),LowerIdent(13:4-13:5),CloseRound(13:5-13:6),
CloseCurly(14:1-14:2),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @1.1-14.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @3.1-3.35 (name "scopedTypeVarInternal")
			(ty-fn @3.25-3.35
				(ty-var @3.25-3.28 (raw "val"))
				(ty-var @3.32-3.35 (raw "val"))))
		(s-decl @4.1-14.2
			(p-ident @4.1-4.22 (raw "scopedTypeVarInternal"))
			(e-lambda @4.25-14.2
				(args
					(p-ident @4.26-4.27 (raw "a")))
				(e-block @4.29-14.2
					(statements
						(s-type-anno @5.2-5.28 (name "b")
							(ty-fn @5.6-5.28
								(ty-var @5.6-5.15 (raw "other_val"))
								(ty-var @5.19-5.28 (raw "other_val"))))
						(s-decl @6.2-11.3
							(p-ident @6.2-6.3 (raw "b"))
							(e-lambda @6.6-11.3
								(args
									(p-ident @6.7-6.8 (raw "c")))
								(e-block @6.10-11.3
									(statements
										(s-type-anno @7.3-7.16 (name "d")
											(ty-var @7.7-7.16 (raw "other_val")))
										(s-decl @8.3-8.8
											(p-ident @8.3-8.4 (raw "d"))
											(e-ident @8.7-8.8 (raw "c")))
										(e-ident @10.3-10.4 (raw "d"))))))
						(e-apply @13.2-13.6
							(e-ident @13.2-13.3 (raw "b"))
							(e-ident @13.4-13.5 (raw "a")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.22 (ident "scopedTypeVarInternal"))
		(e-lambda @4.25-14.2
			(args
				(p-assign @4.26-4.27 (ident "a")))
			(e-block @4.29-14.2
				(s-let @6.2-11.3
					(p-assign @6.2-6.3 (ident "b"))
					(e-lambda @6.6-11.3
						(args
							(p-assign @6.7-6.8 (ident "c")))
						(e-block @6.10-11.3
							(s-let @8.3-8.8
								(p-assign @8.3-8.4 (ident "d"))
								(e-lookup-local @8.7-8.8
									(p-assign @6.7-6.8 (ident "c"))))
							(e-lookup-local @10.3-10.4
								(p-assign @8.3-8.4 (ident "d"))))))
				(e-call @13.2-13.6
					(e-lookup-local @13.2-13.3
						(p-assign @6.2-6.3 (ident "b")))
					(e-lookup-local @13.4-13.5
						(p-assign @4.26-4.27 (ident "a"))))))
		(annotation @4.1-4.22
			(declared-type
				(ty-fn @3.25-3.35 (effectful false)
					(ty-var @3.25-3.28 (name "val"))
					(ty-var @3.32-3.35 (name "val")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.22 (type "val -> val")))
	(expressions
		(expr @4.25-14.2 (type "val -> val"))))
~~~
