# META
~~~ini
description=Derived to_hash on a record with a generic field must add a to_hash requirement to the helper's type, so a function key fails like a direct Dict.insert does (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
insertKey = |k| Dict.empty().insert({ n: k }, 99)

x = insertKey(|z| z)
~~~
# EXPECTED
MISSING METHOD - derived_to_hash_generic_component_issue_11575.md:3:5:3:21
TYPE DOES NOT SUPPORT EQUALITY - derived_to_hash_generic_component_issue_11575.md:3:5:3:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 3 5) (end 3 21))
		(headline
			(reflow "A")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause requires the")
			(reflow " ")
			(annotated code "to_hash")
			(reflow " ")
			(reflow "method here, but the type being used doesn't have that method."))
		(document
			(source-region (file "derived_to_hash_generic_component_issue_11575.md") (start 3 5) (end 3 21) (annotation error) (line-text "x = insertKey(|z| z)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "to_hash")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> a")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Does Not Support Equality")
		(region (start 3 5) (end 3 21))
		(headline
			(reflow "A")
			(reflow " ")
			(annotated code "where")
			(reflow " ")
			(reflow "clause requires equality here, but the type being used doesn't support equality."))
		(document
			(source-region (file "derived_to_hash_generic_component_issue_11575.md") (start 3 5) (end 3 21) (annotation error) (line-text "x = insertKey(|z| z)"))
			(line-break)
			(reflow "The type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> a")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Functions cannot be compared for equality.")
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,Comma,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "insertKey"))
			(e-lambda
				(args
					(p-ident (raw "k")))
				(e-method-call (method ".insert")
					(receiver
						(e-apply
							(e-ident (raw "Dict.empty"))))
					(args
						(e-record
							(field (field "n")
								(e-ident (raw "k"))))
						(e-int (raw "99"))))))
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-ident (raw "insertKey"))
				(e-lambda
					(args
						(p-ident (raw "z")))
					(e-ident (raw "z")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "insertKey"))
		(e-lambda
			(args
				(p-assign (ident "k")))
			(e-dispatch-call (method "insert") (constraint-fn-var 229)
				(receiver
					(e-call (constraint-fn-var 226)
						(e-lookup-external
							(builtin))))
				(args
					(e-record
						(fields
							(field (name "n")
								(e-lookup-local
									(p-assign (ident "k"))))))
					(e-num (value "99"))))))
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Dict({ n: a }, v) where [a.is_eq : a, a -> Bool, a.to_hash : a, Hasher -> Hasher, v.from_numeral : Numeral -> Try(v, [InvalidNumeral(Str)])]"))
		(patt (type "Dict({ n: a -> a }, Dec)")))
	(expressions
		(expr (type "a -> Dict({ n: a }, v) where [a.is_eq : a, a -> Bool, a.to_hash : a, Hasher -> Hasher, v.from_numeral : Numeral -> Try(v, [InvalidNumeral(Str)])]"))
		(expr (type "Dict({ n: a -> a }, Dec)"))))
~~~
