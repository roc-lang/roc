# META
~~~ini
description=Guard: a derived-marker nominal whose template contains a method nominal with a satisfiable where clause keeps checking cleanly; template comparisons are admitted with their formals, not dispatched per use (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
Nullable(a) := [Null, NotNull(a)].{
    is_eq : Nullable(a), Nullable(a) -> Bool where [a.is_eq : a, a -> Bool]
    is_eq = |x, _y| match x {
        NotNull(v) => v == v
        Null => Bool.True
    }
}

Wrap(a) := { inner : Nullable(a) }.{
    is_eq : _
}

x : Wrap(Str)
x = { inner: NotNull("a") }

y = x == x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,NamedUnderscore,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpEquals,LowerIdent,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,Underscore,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpEquals,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Nullable")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty (name "Null"))
					(ty-apply
						(ty (name "NotNull"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "is_eq")
					(ty-fn
						(ty-apply
							(ty (name "Nullable"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Nullable"))
							(ty-var (raw "a")))
						(ty (name "Bool")))
					(where
						(method (mod-of "a") (name "is_eq")
							(ty-fn
								(ty-var (raw "a"))
								(ty-var (raw "a"))
								(ty (name "Bool"))))))
				(s-decl
					(p-ident (raw "is_eq"))
					(e-lambda
						(args
							(p-ident (raw "x"))
							(p-ident (raw "_y")))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "NotNull")
										(p-ident (raw "v")))
									(e-binop (op "==")
										(e-ident (raw "v"))
										(e-ident (raw "v"))))
								(branch
									(p-tag (raw "Null"))
									(e-tag (raw "Bool.True")))))))))
		(s-type-decl
			(header (name "Wrap")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "inner")
					(ty-apply
						(ty (name "Nullable"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "is_eq")
					(_))))
		(s-type-anno (name "x")
			(ty-apply
				(ty (name "Wrap"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "x"))
			(e-record
				(field (field "inner")
					(e-apply
						(e-tag (raw "NotNull"))
						(e-string
							(e-string-part (raw "a")))))))
		(s-decl
			(p-ident (raw "y"))
			(e-binop (op "==")
				(e-ident (raw "x"))
				(e-ident (raw "x"))))))
~~~
# FORMATTED
~~~roc
Nullable(a) := [Null, NotNull(a)].{
	is_eq : Nullable(a), Nullable(a) -> Bool where [a.is_eq : a, a -> Bool]
	is_eq = |x, _y| match x {
		NotNull(v) => v == v
		Null => Bool.True
	}
}

Wrap(a) := { inner : Nullable(a) }.{
	is_eq : _
}

x : Wrap(Str)
x = { inner: NotNull("a") }

y = x == x
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "derived_eq_derived_marker_template_guard_issue_11575.Nullable.is_eq"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "_y")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "x"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "v"))))
									(rhs
										(e-lookup-local
											(p-assign (ident "v")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal-external
									(builtin)
									(e-tag (name "True")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Nullable") (local)
					(ty-rigid-var (name "a")))
				(ty-apply (name "Nullable") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Bool") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "is_eq")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Bool") (builtin)))))))
	(d-let
		(p-assign (ident "derived_eq_derived_marker_template_guard_issue_11575.Wrap.is_eq"))
		(e-derived-method (kind "equality"))
		(annotation
			(ty-underscore)))
	(d-let
		(p-assign (ident "x"))
		(e-record
			(fields
				(field (name "inner")
					(e-tag (name "NotNull")
						(args
							(e-string
								(e-literal (string "a"))))))))
		(annotation
			(ty-apply (name "Wrap") (local)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "y"))
		(e-structural-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "x"))))
			(rhs
				(e-lookup-local
					(p-assign (ident "x"))))))
	(s-nominal-decl
		(ty-header (name "Nullable")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Null"))
			(ty-tag-name (name "NotNull")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-nominal-decl
		(ty-header (name "Wrap")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "inner")
				(ty-apply (name "Nullable") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Nullable(a), Nullable(a) -> Bool where [a.is_eq : a, a -> Bool]"))
		(patt (type "_b"))
		(patt (type "Wrap(Str)"))
		(patt (type "Bool")))
	(type_decls
		(nominal (type "Nullable(a)")
			(ty-header (name "Nullable")
				(ty-args
					(ty-rigid-var (name "a")))))
		(nominal (type "Wrap(a)")
			(ty-header (name "Wrap")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Nullable(a), Nullable(a) -> Bool where [a.is_eq : a, a -> Bool]"))
		(expr (type "_b"))
		(expr (type "Wrap(Str)"))
		(expr (type "Bool"))))
~~~
