# META
~~~ini
description=Derived == on a record must check a nominal field's own is_eq method where clause like a direct comparison does (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
Nullable(a) := [Null, NotNull(a)].{
    is_eq : Nullable(a), Nullable(a) -> Bool where [a.frob : a -> Bool]
    is_eq = |x, _y| match x {
        NotNull(v) => v.frob()
        Null => Bool.True
    }
}

x : { n : Nullable(Str) }
x = { n: NotNull("a") }

y = x == x
~~~
# EXPECTED
MISSING METHOD - derived_eq_nominal_component_where_issue_11575.md:12:5:12:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 12 5) (end 12 11))
		(headline
			(reflow "A")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause requires the")
			(reflow " ")
			(annotated code "frob")
			(reflow " ")
			(reflow "method here, but the type being used doesn't have that method."))
		(document
			(source-region (file "derived_eq_nominal_component_where_issue_11575.md") (start 12 5) (end 12 11) (annotation error) (line-text "y = x == x"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "frob")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "For this to work, the type would need to have a method named")
			(reflow " ")
			(annotated code "frob")
			(reflow " ")
			(reflow "associated with it in the type's declaration."))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,NamedUnderscore,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
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
						(ty (name "Bool")))
					(where
						(method (mod-of "a") (name "frob")
							(ty-fn
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
									(e-method-call (method ".frob")
										(receiver
											(e-ident (raw "v")))
										(args)))
								(branch
									(p-tag (raw "Null"))
									(e-tag (raw "Bool.True")))))))))
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
	is_eq : Nullable(a), Nullable(a) -> Bool where [a.frob : a -> Bool]
	is_eq = |x, _y| match x {
		NotNull(v) => v.frob()
		Null => Bool.True
	}
}

x : { n : Nullable(Str) }
x = { n: NotNull("a") }

y = x == x
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "derived_eq_nominal_component_where_issue_11575.Nullable.is_eq"))
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
								(e-dispatch-call (method "frob") (constraint-fn-var 295)
									(receiver
										(e-lookup-local
											(p-assign (ident "v"))))
									(args))))
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
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "frob")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Bool") (builtin)))))))
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
		(e-runtime-error (tag "erroneous_value_expr")))
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
		(patt (type "Nullable(a), Nullable(a) -> Bool where [a.frob : a -> Bool]"))
		(patt (type "{ n: Nullable(Str) }"))
		(patt (type "Bool")))
	(type_decls
		(nominal (type "Nullable(a)")
			(ty-header (name "Nullable")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Nullable(a), Nullable(a) -> Bool where [a.frob : a -> Bool]"))
		(expr (type "{ n: Nullable(Str) }"))
		(expr (type "Bool"))))
~~~
