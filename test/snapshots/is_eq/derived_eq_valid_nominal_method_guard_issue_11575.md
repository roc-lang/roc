# META
~~~ini
description=Guard: derived == on a record of a nominal with a valid where-free is_eq method keeps checking cleanly (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
Nullable(a) := [Null, NotNull(a)].{
    is_eq : Nullable(a), Nullable(a) -> Bool
    is_eq = |_, _y| Bool.True
}

x : { n : Nullable(Str) }
x = { n: NotNull("a") }

y = x == x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,Comma,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
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
						(ty (name "Bool"))))
				(s-decl
					(p-ident (raw "is_eq"))
					(e-lambda
						(args
							(p-underscore)
							(p-ident (raw "_y")))
						(e-tag (raw "Bool.True"))))))
		(s-type-anno (name "x")
			(ty-record
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "Nullable"))
						(ty (name "Str"))))))
		(s-decl
			(p-ident (raw "x"))
			(e-record
				(field (field "n")
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
	is_eq : Nullable(a), Nullable(a) -> Bool
	is_eq = |_, _y| Bool.True
}

x : { n : Nullable(Str) }
x = { n: NotNull("a") }

y = x == x
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "derived_eq_valid_nominal_method_guard_issue_11575.Nullable.is_eq"))
		(e-lambda
			(args
				(p-underscore)
				(p-assign (ident "_y")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Nullable") (local)
					(ty-rigid-var (name "a")))
				(ty-apply (name "Nullable") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "x"))
		(e-record
			(fields
				(field (name "n")
					(e-tag (name "NotNull")
						(args
							(e-string
								(e-literal (string "a"))))))))
		(annotation
			(ty-record
				(field (field "n")
					(ty-apply (name "Nullable") (local)
						(ty-lookup (name "Str") (builtin)))))))
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
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Nullable(a), Nullable(a) -> Bool"))
		(patt (type "{ n: Nullable(Str) }"))
		(patt (type "Bool")))
	(type_decls
		(nominal (type "Nullable(a)")
			(ty-header (name "Nullable")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Nullable(a), Nullable(a) -> Bool"))
		(expr (type "{ n: Nullable(Str) }"))
		(expr (type "Bool"))))
~~~
