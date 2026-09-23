# META
~~~ini
description=Derived to_hash on a record must check a nominal field's own to_hash method where clause like a direct Dict.insert does (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
Nullable(a) := [Null, NotNull(a)].{
    is_eq : Nullable(a), Nullable(a) -> Bool
    is_eq = |_, _y| Bool.True
    to_hash : Nullable(a), Hasher -> Hasher where [a.frob : a, Hasher -> Hasher]
    to_hash = |x, hasher| match x {
        NotNull(v) => v.frob(hasher)
        Null => hasher
    }
}

k : { n : Nullable(Str) }
k = { n: NotNull("a") }

y = Dict.empty().insert(k, 99)
~~~
# EXPECTED
MISSING METHOD - derived_to_hash_nominal_component_issue_11575.md:14:5:14:31
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 14 5) (end 14 31))
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
			(source-region (file "derived_to_hash_nominal_component_issue_11575.md") (start 14 5) (end 14 31) (annotation error) (line-text "y = Dict.empty().insert(k, 99)"))
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
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,Comma,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
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
						(e-tag (raw "Bool.True"))))
				(s-type-anno (name "to_hash")
					(ty-fn
						(ty-apply
							(ty (name "Nullable"))
							(ty-var (raw "a")))
						(ty (name "Hasher"))
						(ty (name "Hasher")))
					(where
						(method (mod-of "a") (name "frob")
							(ty-fn
								(ty-var (raw "a"))
								(ty (name "Hasher"))
								(ty (name "Hasher"))))))
				(s-decl
					(p-ident (raw "to_hash"))
					(e-lambda
						(args
							(p-ident (raw "x"))
							(p-ident (raw "hasher")))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "NotNull")
										(p-ident (raw "v")))
									(e-method-call (method ".frob")
										(receiver
											(e-ident (raw "v")))
										(args
											(e-ident (raw "hasher")))))
								(branch
									(p-tag (raw "Null"))
									(e-ident (raw "hasher")))))))))
		(s-type-anno (name "k")
			(ty-record
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "Nullable"))
						(ty (name "Str"))))))
		(s-decl
			(p-ident (raw "k"))
			(e-record
				(field (field "n")
					(e-apply
						(e-tag (raw "NotNull"))
						(e-string
							(e-string-part (raw "a")))))))
		(s-decl
			(p-ident (raw "y"))
			(e-method-call (method ".insert")
				(receiver
					(e-apply
						(e-ident (raw "Dict.empty"))))
				(args
					(e-ident (raw "k"))
					(e-int (raw "99")))))))
~~~
# FORMATTED
~~~roc
Nullable(a) := [Null, NotNull(a)].{
	is_eq : Nullable(a), Nullable(a) -> Bool
	is_eq = |_, _y| Bool.True
	to_hash : Nullable(a), Hasher -> Hasher where [a.frob : a, Hasher -> Hasher]
	to_hash = |x, hasher| match x {
		NotNull(v) => v.frob(hasher)
		Null => hasher
	}
}

k : { n : Nullable(Str) }
k = { n: NotNull("a") }

y = Dict.empty().insert(k, 99)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "derived_to_hash_nominal_component_issue_11575.Nullable.is_eq"))
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
		(p-assign (ident "derived_to_hash_nominal_component_issue_11575.Nullable.to_hash"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "hasher")))
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
								(e-dispatch-call (method "frob") (constraint-fn-var 341)
									(receiver
										(e-lookup-local
											(p-assign (ident "v"))))
									(args
										(e-lookup-local
											(p-assign (ident "hasher")))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "hasher")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Nullable") (local)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "Hasher") (builtin))
				(ty-lookup (name "Hasher") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "frob")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Hasher") (builtin))
						(ty-lookup (name "Hasher") (builtin)))))))
	(d-let
		(p-assign (ident "k"))
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
		(patt (type "Nullable(a), Nullable(a) -> Bool"))
		(patt (type "Nullable(a), Hasher -> Hasher where [a.frob : a, Hasher -> Hasher]"))
		(patt (type "{ n: Nullable(Str) }"))
		(patt (type "Dict({ n: Nullable(Str) }, Dec)")))
	(type_decls
		(nominal (type "Nullable(a)")
			(ty-header (name "Nullable")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Nullable(a), Nullable(a) -> Bool"))
		(expr (type "Nullable(a), Hasher -> Hasher where [a.frob : a, Hasher -> Hasher]"))
		(expr (type "{ n: Nullable(Str) }"))
		(expr (type "Dict({ n: Nullable(Str) }, Dec)"))))
~~~
