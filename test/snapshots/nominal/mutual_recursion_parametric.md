# META
~~~ini
description=Mutually recursive parametric functions (associated items) should type-check (issue #9491 follow-up)
type=file:RBMut.roc
~~~
# SOURCE
~~~roc
RBMut(k) := [
	Empty,
	Node(RBMut(k)),
].{
	delA : RBMut(k) -> RBMut(k)
	delA = |inner| match inner {
		RBMut.Node(x) => x->delB()
		Empty => Empty
	}
	delB : RBMut(k) -> RBMut(k)
	delB = |t| match t {
		RBMut.Node(inner) => inner->delA()
		_ => t
	}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,
UpperIdent,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,Comma,
CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
Underscore,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "RBMut")
				(args
					(ty-var (raw "k"))))
			(ty-tag-union
				(tags
					(ty (name "Empty"))
					(ty-apply
						(ty (name "Node"))
						(ty-apply
							(ty (name "RBMut"))
							(ty-var (raw "k"))))))
			(associated
				(s-type-anno (name "delA")
					(ty-fn
						(ty-apply
							(ty (name "RBMut"))
							(ty-var (raw "k")))
						(ty-apply
							(ty (name "RBMut"))
							(ty-var (raw "k")))))
				(s-decl
					(p-ident (raw "delA"))
					(e-lambda
						(args
							(p-ident (raw "inner")))
						(e-match
							(e-ident (raw "inner"))
							(branches
								(branch
									(p-tag (raw ".Node")
										(p-ident (raw "x")))
									(e-arrow-call
										(e-ident (raw "x"))
										(e-apply
											(e-ident (raw "delB")))))
								(branch
									(p-tag (raw "Empty"))
									(e-tag (raw "Empty")))))))
				(s-type-anno (name "delB")
					(ty-fn
						(ty-apply
							(ty (name "RBMut"))
							(ty-var (raw "k")))
						(ty-apply
							(ty (name "RBMut"))
							(ty-var (raw "k")))))
				(s-decl
					(p-ident (raw "delB"))
					(e-lambda
						(args
							(p-ident (raw "t")))
						(e-match
							(e-ident (raw "t"))
							(branches
								(branch
									(p-tag (raw ".Node")
										(p-ident (raw "inner")))
									(e-arrow-call
										(e-ident (raw "inner"))
										(e-apply
											(e-ident (raw "delA")))))
								(branch
									(p-underscore)
									(e-ident (raw "t")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "RBMut.delA"))
		(e-lambda
			(args
				(p-assign (ident "inner")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "inner"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-call (constraint-fn-var 127)
									(e-lookup-local
										(p-assign (ident "RBMut.delB")))
									(e-lookup-local
										(p-assign (ident "x"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "Empty"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "RBMut") (local)
					(ty-rigid-var (name "k")))
				(ty-apply (name "RBMut") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "k")))))))
	(d-let
		(p-assign (ident "RBMut.delB"))
		(e-lambda
			(args
				(p-assign (ident "t")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "t"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-call (constraint-fn-var 124)
									(e-lookup-local
										(p-assign (ident "RBMut.delA")))
									(e-lookup-local
										(p-assign (ident "inner"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-lookup-local
									(p-assign (ident "t")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "RBMut") (local)
					(ty-rigid-var (name "k")))
				(ty-apply (name "RBMut") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "k")))))))
	(s-nominal-decl
		(ty-header (name "RBMut")
			(ty-args
				(ty-rigid-var (name "k"))))
		(ty-tag-union
			(ty-tag-name (name "Empty"))
			(ty-tag-name (name "Node")
				(ty-apply (name "RBMut") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "k"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "RBMut(k) -> RBMut(k)"))
		(patt (type "RBMut(k) -> RBMut(k)")))
	(type_decls
		(nominal (type "RBMut(k)")
			(ty-header (name "RBMut")
				(ty-args
					(ty-rigid-var (name "k"))))))
	(expressions
		(expr (type "RBMut(k) -> RBMut(k)"))
		(expr (type "RBMut(k) -> RBMut(k)"))))
~~~
