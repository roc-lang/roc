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
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,
CloseCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "scopedTypeVarInternal")
			(ty-fn
				(ty-var (raw "val"))
				(ty-var (raw "val"))))
		(s-decl
			(p-ident (raw "scopedTypeVarInternal"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-block
					(statements
						(s-type-anno (name "b")
							(ty-fn
								(ty-var (raw "other_val"))
								(ty-var (raw "other_val"))))
						(s-decl
							(p-ident (raw "b"))
							(e-lambda
								(args
									(p-ident (raw "c")))
								(e-block
									(statements
										(s-type-anno (name "d")
											(ty-var (raw "other_val")))
										(s-decl
											(p-ident (raw "d"))
											(e-ident (raw "c")))
										(e-ident (raw "d"))))))
						(e-apply
							(e-ident (raw "b"))
							(e-ident (raw "a")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "scopedTypeVarInternal"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-block
				(s-let
					(p-assign (ident "b"))
					(e-lambda
						(args
							(p-assign (ident "c")))
						(e-block
							(s-let
								(p-assign (ident "d"))
								(e-lookup-local
									(p-assign (ident "c"))))
							(e-lookup-local
								(p-assign (ident "d"))))))
				(e-call
					(e-lookup-local
						(p-assign (ident "b")))
					(e-lookup-local
						(p-assign (ident "a"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "val"))
				(ty-rigid-var-lookup (ty-rigid-var (name "val")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "val -> val")))
	(expressions
		(expr (type "val -> val"))))
~~~
