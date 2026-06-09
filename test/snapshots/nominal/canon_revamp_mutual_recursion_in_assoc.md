# META
~~~ini
description=Mutually recursive methods within an associated block - each refs the other via unqualified name
type=file:Tree.roc
~~~
# SOURCE
~~~roc
Tree := [Leaf, Node].{
    isEven : Tree -> Tree
    isEven = |t| isOdd(t)

    isOdd : Tree -> Tree
    isOdd = |t| isEven(t)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Tree")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Leaf"))
					(ty (name "Node"))))
			(associated
				(s-type-anno (name "isEven")
					(ty-fn
						(ty (name "Tree"))
						(ty (name "Tree"))))
				(s-decl
					(p-ident (raw "isEven"))
					(e-lambda
						(args
							(p-ident (raw "t")))
						(e-apply
							(e-ident (raw "isOdd"))
							(e-ident (raw "t")))))
				(s-type-anno (name "isOdd")
					(ty-fn
						(ty (name "Tree"))
						(ty (name "Tree"))))
				(s-decl
					(p-ident (raw "isOdd"))
					(e-lambda
						(args
							(p-ident (raw "t")))
						(e-apply
							(e-ident (raw "isEven"))
							(e-ident (raw "t")))))))))
~~~
# FORMATTED
~~~roc
Tree := [Leaf, Node].{
	isEven : Tree -> Tree
	isEven = |t| isOdd(t)

	isOdd : Tree -> Tree
	isOdd = |t| isEven(t)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Tree.isEven"))
		(e-lambda
			(args
				(p-assign (ident "t")))
			(e-call (constraint-fn-var 67)
				(e-lookup-local
					(p-assign (ident "Tree.isOdd")))
				(e-lookup-local
					(p-assign (ident "t")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Tree") (local))
				(ty-lookup (name "Tree") (local)))))
	(d-let
		(p-assign (ident "Tree.isOdd"))
		(e-lambda
			(args
				(p-assign (ident "t")))
			(e-call (constraint-fn-var 64)
				(e-lookup-local
					(p-assign (ident "Tree.isEven")))
				(e-lookup-local
					(p-assign (ident "t")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Tree") (local))
				(ty-lookup (name "Tree") (local)))))
	(s-nominal-decl
		(ty-header (name "Tree"))
		(ty-tag-union
			(ty-tag-name (name "Leaf"))
			(ty-tag-name (name "Node")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Tree -> Tree"))
		(patt (type "Tree -> Tree")))
	(type_decls
		(nominal (type "Tree")
			(ty-header (name "Tree"))))
	(expressions
		(expr (type "Tree -> Tree"))
		(expr (type "Tree -> Tree"))))
~~~
