# META
~~~ini
description=Derived == on a record with a generic field must add an is_eq requirement to the helper's type, so calling it with a function fails like a direct comparison does (issue 11575)
type=snippet
~~~
# SOURCE
~~~roc
eq = |a, b| { n: a } == { n: b }

x = eq(|z| z, |z| z)
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - derived_eq_generic_component_via_helper_issue_11575.md:1:13:1:33
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Does Not Support Equality")
		(region (start 1 13) (end 1 33))
		(headline
			(reflow "This expression is doing an equality check on a type that doesn't support equality."))
		(document
			(source-region (file "derived_eq_generic_component_via_helper_issue_11575.md") (start 1 13) (end 1 33) (annotation error) (line-text "eq = |a, b| { n: a } == { n: b }"))
			(line-break)
			(reflow "The type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "c -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Functions cannot be compared for equality.")
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,OpEquals,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,Comma,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "eq"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-binop (op "==")
					(e-record
						(field (field "n")
							(e-ident (raw "a"))))
					(e-record
						(field (field "n")
							(e-ident (raw "b")))))))
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-ident (raw "eq"))
				(e-lambda
					(args
						(p-ident (raw "z")))
					(e-ident (raw "z")))
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
		(p-assign (ident "eq"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-runtime-error (tag "erroneous_value_expr"))))
	(d-let
		(p-assign (ident "x"))
		(e-call (constraint-fn-var 240)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-lambda
				(args
					(p-assign (ident "z")))
				(e-lookup-local
					(p-assign (ident "z"))))
			(e-lambda
				(args
					(p-assign (ident "z")))
				(e-lookup-local
					(p-assign (ident "z")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, c -> Bool where [c.is_eq : c, c -> Bool]"))
		(patt (type "Bool")))
	(expressions
		(expr (type "c, c -> Bool where [c.is_eq : c, c -> Bool]"))
		(expr (type "Bool"))))
~~~
