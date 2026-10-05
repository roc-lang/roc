# META
~~~ini
description=Derived == and to_hash on open tag rows give the row tail an is_eq or to_hash requirement; an annotation declares it, and a top-level value's tail closes to the empty row
type=snippet
~~~
# SOURCE
~~~roc
same = |a, b| if a == Nope { False } else { a == b }

is_nope : [Nope, ..a] -> Bool where [a.is_eq : a, a -> Bool]
is_nope = |v| v == Nope

value = Some("a")

checks = (same(Yes(1), Yes(1)), is_nope(Yes(2)), value == None)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,UpperIdent,OpenCurly,UpperIdent,CloseCurly,KwElse,OpenCurly,LowerIdent,OpEquals,LowerIdent,CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpEquals,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,Comma,LowerIdent,OpEquals,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "same"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "a"))
						(e-tag (raw "Nope")))
					(e-block
						(statements
							(e-tag (raw "False"))))
					(e-block
						(statements
							(e-binop (op "==")
								(e-ident (raw "a"))
								(e-ident (raw "b"))))))))
		(s-type-anno (name "is_nope")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "Nope")))
					(ty-var (raw "a")))
				(ty (name "Bool")))
			(where
				(method (mod-of "a") (name "is_eq")
					(ty-fn
						(ty-var (raw "a"))
						(ty-var (raw "a"))
						(ty (name "Bool"))))))
		(s-decl
			(p-ident (raw "is_nope"))
			(e-lambda
				(args
					(p-ident (raw "v")))
				(e-binop (op "==")
					(e-ident (raw "v"))
					(e-tag (raw "Nope")))))
		(s-decl
			(p-ident (raw "value"))
			(e-apply
				(e-tag (raw "Some"))
				(e-string
					(e-string-part (raw "a")))))
		(s-decl
			(p-ident (raw "checks"))
			(e-tuple
				(e-apply
					(e-ident (raw "same"))
					(e-apply
						(e-tag (raw "Yes"))
						(e-int (raw "1")))
					(e-apply
						(e-tag (raw "Yes"))
						(e-int (raw "1"))))
				(e-apply
					(e-ident (raw "is_nope"))
					(e-apply
						(e-tag (raw "Yes"))
						(e-int (raw "2"))))
				(e-binop (op "==")
					(e-ident (raw "value"))
					(e-tag (raw "None")))))))
~~~
# FORMATTED
~~~roc
same = |a, b| if a == Nope {
	False
} else {
	a == b
}

is_nope : [Nope, ..a] -> Bool where [a.is_eq : a, a -> Bool]
is_nope = |v| v == Nope

value = Some("a")

checks = (same(Yes(1), Yes(1)), is_nope(Yes(2)), value == None)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "same"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-if
				(if-branches
					(if-branch
						(e-structural-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "a"))))
							(rhs
								(e-tag (name "Nope"))))
						(e-block
							(e-tag (name "False")))))
				(if-else
					(e-block
						(e-structural-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "a"))))
							(rhs
								(e-lookup-local
									(p-assign (ident "b"))))))))))
	(d-let
		(p-assign (ident "is_nope"))
		(e-lambda
			(args
				(p-assign (ident "v")))
			(e-structural-eq (negated "false")
				(lhs
					(e-lookup-local
						(p-assign (ident "v"))))
				(rhs
					(e-tag (name "Nope")))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Nope"))
					(ty-rigid-var (name "a")))
				(ty-lookup (name "Bool") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "is_eq")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Bool") (builtin)))))))
	(d-let
		(p-assign (ident "value"))
		(e-tag (name "Some")
			(args
				(e-string
					(e-literal (string "a"))))))
	(d-let
		(p-assign (ident "checks"))
		(e-tuple
			(elems
				(e-call (constraint-fn-var 353)
					(e-lookup-local
						(p-assign (ident "same")))
					(e-tag (name "Yes")
						(args
							(e-num (value "1"))))
					(e-tag (name "Yes")
						(args
							(e-num (value "1")))))
				(e-call (constraint-fn-var 384)
					(e-lookup-local
						(p-assign (ident "is_nope")))
					(e-tag (name "Yes")
						(args
							(e-num (value "2")))))
				(e-structural-eq (negated "false")
					(lhs
						(e-lookup-local
							(p-assign (ident "value"))))
					(rhs
						(e-tag (name "None"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Nope, ..c], [Nope, ..c] -> Bool where [c.is_eq : c, c -> Bool]"))
		(patt (type "[Nope, ..a] -> Bool where [a.is_eq : a, a -> Bool]"))
		(patt (type "[None, Some(Str)]"))
		(patt (type "(Bool, Bool, Bool)")))
	(expressions
		(expr (type "[Nope, ..c], [Nope, ..c] -> Bool where [c.is_eq : c, c -> Bool]"))
		(expr (type "[Nope, ..a] -> Bool where [a.is_eq : a, a -> Bool]"))
		(expr (type "[None, Some(Str)]"))
		(expr (type "(Bool, Bool, Bool)"))))
~~~
