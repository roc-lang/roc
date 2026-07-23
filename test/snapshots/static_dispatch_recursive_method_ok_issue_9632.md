# META
~~~ini
description=Recursive method calls through data fields of the annotated nominal type stay accepted (issue 9632)
type=file
~~~
# SOURCE
~~~roc
Tree := [Leaf, Node({ value : U64, rest : Tree })].{
    total : Tree -> U64
    total = |tree| {
        match tree {
            Leaf => 0
            Node(node) => node.rest.total() + node.value
        }
    }
}

empty : Tree
empty = Tree.Leaf

result = empty.total()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceDotLowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpPlus,LowerIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Tree")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Leaf"))
					(ty-apply
						(ty (name "Node"))
						(ty-record
							(anno-record-field (name "value")
								(ty (name "U64")))
							(anno-record-field (name "rest")
								(ty (name "Tree")))))))
			(associated
				(s-type-anno (name "total")
					(ty-fn
						(ty (name "Tree"))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "total"))
					(e-lambda
						(args
							(p-ident (raw "tree")))
						(e-block
							(statements
								(e-match
									(e-ident (raw "tree"))
									(branches
										(branch
											(p-tag (raw "Leaf"))
											(e-int (raw "0")))
										(branch
											(p-tag (raw "Node")
												(p-ident (raw "node")))
											(e-binop (op "+")
												(e-method-call (method ".total")
													(receiver
														(e-field-access
															(e-ident (raw "node"))
															(e-ident (raw "rest"))))
													(args))
												(e-field-access
													(e-ident (raw "node"))
													(e-ident (raw "value")))))))))))))
		(s-type-anno (name "empty")
			(ty (name "Tree")))
		(s-decl
			(p-ident (raw "empty"))
			(e-tag (raw "Tree.Leaf")))
		(s-decl
			(p-ident (raw "result"))
			(e-method-call (method ".total")
				(receiver
					(e-ident (raw "empty")))
				(args)))))
~~~
# FORMATTED
~~~roc
Tree := [Leaf, Node({ value : U64, rest : Tree })].{
	total : Tree -> U64
	total = |tree| {
		match tree {
			Leaf => 0
			Node(node) => node.rest.total() + node.value
		}
	}
}

empty : Tree
empty = Tree.Leaf

result = empty.total()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "static_dispatch_recursive_method_ok_issue_9632.Tree.total"))
		(e-lambda
			(args
				(p-assign (ident "tree")))
			(e-block
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "tree"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-num (value "0"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-dispatch-call (method "plus") (constraint-fn-var 266)
										(receiver
											(e-dispatch-call (method "total") (constraint-fn-var 260)
												(receiver
													(e-field-access (field "rest")
														(receiver
															(e-lookup-local
																(p-assign (ident "node"))))))
												(args)))
										(args
											(e-field-access (field "value")
												(receiver
													(e-lookup-local
														(p-assign (ident "node"))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Tree") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "empty"))
		(e-nominal (nominal "Tree")
			(e-tag (name "Leaf")))
		(annotation
			(ty-lookup (name "Tree") (local))))
	(d-let
		(p-assign (ident "result"))
		(e-dispatch-call (method "total") (constraint-fn-var 310)
			(receiver
				(e-lookup-local
					(p-assign (ident "empty"))))
			(args)))
	(s-nominal-decl
		(ty-header (name "Tree"))
		(ty-tag-union
			(ty-tag-name (name "Leaf"))
			(ty-tag-name (name "Node")
				(ty-record
					(field (field "value")
						(ty-lookup (name "U64") (builtin)))
					(field (field "rest")
						(ty-lookup (name "Tree") (local))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Tree -> U64"))
		(patt (type "Tree"))
		(patt (type "U64")))
	(type_decls
		(nominal (type "Tree")
			(ty-header (name "Tree"))))
	(expressions
		(expr (type "Tree -> U64"))
		(expr (type "Tree"))
		(expr (type "U64"))))
~~~
