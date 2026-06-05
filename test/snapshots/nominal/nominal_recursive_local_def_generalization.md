# META
~~~ini
description=Parametric recursive nominal type with local recursive function defs generalizes correctly (issue #9491)
type=file:RBTree.roc
~~~
# SOURCE
~~~roc
RBTree(k) := [
	Empty,
	Node(RBTree(k)),
].{
	delete = |tree| {
		delRBTree : RBTree(k) -> RBTree(k)
		delRBTree = |inner| {
			match inner {
				RBTree.Node(Empty) => Empty
				RBTree.Node(RBTree.Node(x)) => RBTree.Node(x)->delRBTree()
				Empty => Empty
			}
		}
		delCurr : RBTree(k) -> RBTree(k)
		delCurr = |t| {
			match t {
				RBTree.Node(inner) => inner->delRBTree()
				_ => t
			}
		}
		tree->delCurr()
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,UpperIdent,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
Underscore,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
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
			(header (name "RBTree")
				(args
					(ty-var (raw "k"))))
			(ty-tag-union
				(tags
					(ty (name "Empty"))
					(ty-apply
						(ty (name "Node"))
						(ty-apply
							(ty (name "RBTree"))
							(ty-var (raw "k"))))))
			(associated
				(s-decl
					(p-ident (raw "delete"))
					(e-lambda
						(args
							(p-ident (raw "tree")))
						(e-block
							(statements
								(s-type-anno (name "delRBTree")
									(ty-fn
										(ty-apply
											(ty (name "RBTree"))
											(ty-var (raw "k")))
										(ty-apply
											(ty (name "RBTree"))
											(ty-var (raw "k")))))
								(s-decl
									(p-ident (raw "delRBTree"))
									(e-lambda
										(args
											(p-ident (raw "inner")))
										(e-block
											(statements
												(e-match
													(e-ident (raw "inner"))
													(branches
														(branch
															(p-tag (raw ".Node")
																(p-tag (raw "Empty")))
															(e-tag (raw "Empty")))
														(branch
															(p-tag (raw ".Node")
																(p-tag (raw ".Node")
																	(p-ident (raw "x"))))
															(e-arrow-call
																(e-apply
																	(e-tag (raw "RBTree.Node"))
																	(e-ident (raw "x")))
																(e-apply
																	(e-ident (raw "delRBTree")))))
														(branch
															(p-tag (raw "Empty"))
															(e-tag (raw "Empty")))))))))
								(s-type-anno (name "delCurr")
									(ty-fn
										(ty-apply
											(ty (name "RBTree"))
											(ty-var (raw "k")))
										(ty-apply
											(ty (name "RBTree"))
											(ty-var (raw "k")))))
								(s-decl
									(p-ident (raw "delCurr"))
									(e-lambda
										(args
											(p-ident (raw "t")))
										(e-block
											(statements
												(e-match
													(e-ident (raw "t"))
													(branches
														(branch
															(p-tag (raw ".Node")
																(p-ident (raw "inner")))
															(e-arrow-call
																(e-ident (raw "inner"))
																(e-apply
																	(e-ident (raw "delRBTree")))))
														(branch
															(p-underscore)
															(e-ident (raw "t")))))))))
								(e-arrow-call
									(e-ident (raw "tree"))
									(e-apply
										(e-ident (raw "delCurr"))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "RBTree.delete"))
		(e-lambda
			(args
				(p-assign (ident "tree")))
			(e-block
				(s-let
					(p-assign (ident "delRBTree"))
					(e-lambda
						(args
							(p-assign (ident "inner")))
						(e-block
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
												(e-tag (name "Empty"))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-nominal
														(p-applied-tag))))
											(value
												(e-call (constraint-fn-var 141)
													(e-lookup-local
														(p-assign (ident "delRBTree")))
													(e-nominal (nominal "RBTree")
														(e-tag (name "Node")
															(args
																(e-lookup-local
																	(p-assign (ident "x")))))))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-tag (name "Empty"))))))))))
				(s-let
					(p-assign (ident "delCurr"))
					(e-lambda
						(args
							(p-assign (ident "t")))
						(e-block
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
												(e-call (constraint-fn-var 182)
													(e-lookup-local
														(p-assign (ident "delRBTree")))
													(e-lookup-local
														(p-assign (ident "inner"))))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-underscore)))
											(value
												(e-lookup-local
													(p-assign (ident "t")))))))))))
				(e-call (constraint-fn-var 188)
					(e-lookup-local
						(p-assign (ident "delCurr")))
					(e-lookup-local
						(p-assign (ident "tree")))))))
	(s-nominal-decl
		(ty-header (name "RBTree")
			(ty-args
				(ty-rigid-var (name "k"))))
		(ty-tag-union
			(ty-tag-name (name "Empty"))
			(ty-tag-name (name "Node")
				(ty-apply (name "RBTree") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "k"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "RBTree(k) -> RBTree(k)")))
	(type_decls
		(nominal (type "RBTree(k)")
			(ty-header (name "RBTree")
				(ty-args
					(ty-rigid-var (name "k"))))))
	(expressions
		(expr (type "RBTree(k) -> RBTree(k)"))))
~~~
