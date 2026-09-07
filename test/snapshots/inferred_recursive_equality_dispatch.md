# META
~~~ini
description=Inferred recursive equality closes a concrete method requirement
type=snippet
~~~
# SOURCE
~~~roc
Expr := [Leaf(Str), Next(Expr)].{
    is_eq = |self, other|
        match (self, other) {
            (Leaf(left), Leaf(right)) => left == right
            (Next(left), Next(right)) => left == right
            _ => False
        }
}

make : {} -> Expr
make = |_| Leaf("a")

main = Leaf("a") == make({})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
KwMatch,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpenCurly,
OpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,LowerIdent,OpEquals,LowerIdent,
OpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,LowerIdent,OpEquals,LowerIdent,
Underscore,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpEquals,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Expr")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Leaf"))
						(ty (name "Str")))
					(ty-apply
						(ty (name "Next"))
						(ty (name "Expr")))))
			(associated
				(s-decl
					(p-ident (raw "is_eq"))
					(e-lambda
						(args
							(p-ident (raw "self"))
							(p-ident (raw "other")))
						(e-match
							(e-tuple
								(e-ident (raw "self"))
								(e-ident (raw "other")))
							(branches
								(branch
									(p-tuple
										(p-tag (raw "Leaf")
											(p-ident (raw "left")))
										(p-tag (raw "Leaf")
											(p-ident (raw "right"))))
									(e-binop (op "==")
										(e-ident (raw "left"))
										(e-ident (raw "right"))))
								(branch
									(p-tuple
										(p-tag (raw "Next")
											(p-ident (raw "left")))
										(p-tag (raw "Next")
											(p-ident (raw "right"))))
									(e-binop (op "==")
										(e-ident (raw "left"))
										(e-ident (raw "right"))))
								(branch
									(p-underscore)
									(e-tag (raw "False")))))))))
		(s-type-anno (name "make")
			(ty-fn
				(ty-record)
				(ty (name "Expr"))))
		(s-decl
			(p-ident (raw "make"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Leaf"))
					(e-string
						(e-string-part (raw "a"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-binop (op "==")
				(e-apply
					(e-tag (raw "Leaf"))
					(e-string
						(e-string-part (raw "a"))))
				(e-apply
					(e-ident (raw "make"))
					(e-record))))))
~~~
# FORMATTED
~~~roc
Expr := [Leaf(Str), Next(Expr)].{
	is_eq = |self, other|
		match (self, other) {
			(Leaf(left), Leaf(right)) => left == right
			(Next(left), Next(right)) => left == right
			_ => False
		}
}

make : {} -> Expr
make = |_| Leaf("a")

main = Leaf("a") == make({})
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "inferred_recursive_equality_dispatch.Expr.is_eq"))
		(e-lambda
			(args
				(p-assign (ident "self"))
				(p-assign (ident "other")))
			(e-match
				(match
					(cond
						(e-tuple
							(elems
								(e-lookup-local
									(p-assign (ident "self")))
								(e-lookup-local
									(p-assign (ident "other"))))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-tuple
										(patterns
											(p-applied-tag)
											(p-applied-tag)))))
							(value
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "left"))))
									(rhs
										(e-lookup-local
											(p-assign (ident "right")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-tuple
										(patterns
											(p-applied-tag)
											(p-applied-tag)))))
							(value
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "left"))))
									(rhs
										(e-lookup-local
											(p-assign (ident "right")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-tag (name "False")))))))))
	(d-let
		(p-assign (ident "make"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Leaf")
				(args
					(e-string
						(e-literal (string "a"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-lookup (name "Expr") (local)))))
	(d-let
		(p-assign (ident "main"))
		(e-method-eq (negated "false")
			(lhs
				(e-tag (name "Leaf")
					(args
						(e-string
							(e-literal (string "a"))))))
			(rhs
				(e-call (constraint-fn-var 323)
					(e-lookup-local
						(p-assign (ident "make")))
					(e-empty_record)))))
	(s-nominal-decl
		(ty-header (name "Expr"))
		(ty-tag-union
			(ty-tag-name (name "Leaf")
				(ty-lookup (name "Str") (builtin)))
			(ty-tag-name (name "Next")
				(ty-lookup (name "Expr") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Leaf(a), Next(b), ..], [Leaf(a), Next(b), ..] -> Bool where [a.is_eq : a, a -> Bool, b.is_eq : b, b -> Bool]"))
		(patt (type "{} -> Expr"))
		(patt (type "Bool")))
	(type_decls
		(nominal (type "Expr")
			(ty-header (name "Expr"))))
	(expressions
		(expr (type "[Leaf(a), Next(b), ..], [Leaf(a), Next(b), ..] -> Bool where [a.is_eq : a, a -> Bool, b.is_eq : b, b -> Bool]"))
		(expr (type "{} -> Expr"))
		(expr (type "Bool"))))
~~~
