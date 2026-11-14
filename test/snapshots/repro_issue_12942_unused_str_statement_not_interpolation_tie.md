# META
~~~ini
description=An unused Str statement after a call to a function that interpolates is reported as an unused value, not as that function's interpolation getting conflicting values
type=file
~~~
# SOURCE
~~~roc
Main := [].{
	tool : Str -> Try(Str, _)
	tool = |name| Ok(name)

	served : Str -> Try(Str, _)
	served = |name| Err(Failed("${name}: ${name}"))

	realise : Str -> Try(Str, _)
	realise = |name| {
		_ = Main.served(name)?
		Main.tool(name)?
		Ok(name)
	}
}
~~~
# EXPECTED
TYPE MISMATCH - repro_issue_12942_unused_str_statement_not_interpolation_tie.md:11:3:11:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 11 3) (end 11 19))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "repro_issue_12942_unused_str_statement_not_interpolation_tie.md") (start 11 3) (end 11 19) (annotation error) (line-text "\t\tMain.tool(name)?"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow "."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Main")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-anno (name "tool")
					(ty-fn
						(ty (name "Str"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(_))))
				(s-decl
					(p-ident (raw "tool"))
					(e-lambda
						(args
							(p-ident (raw "name")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "name")))))
				(s-type-anno (name "served")
					(ty-fn
						(ty (name "Str"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(_))))
				(s-decl
					(p-ident (raw "served"))
					(e-lambda
						(args
							(p-ident (raw "name")))
						(e-apply
							(e-tag (raw "Err"))
							(e-apply
								(e-tag (raw "Failed"))
								(e-string
									(e-string-part (raw ""))
									(e-ident (raw "name"))
									(e-string-part (raw ": "))
									(e-ident (raw "name"))
									(e-string-part (raw "")))))))
				(s-type-anno (name "realise")
					(ty-fn
						(ty (name "Str"))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Str"))
							(_))))
				(s-decl
					(p-ident (raw "realise"))
					(e-lambda
						(args
							(p-ident (raw "name")))
						(e-block
							(statements
								(s-decl
									(p-underscore)
									(e-question-suffix
										(e-apply
											(e-ident (raw "Main.served"))
											(e-ident (raw "name")))))
								(e-question-suffix
									(e-apply
										(e-ident (raw "Main.tool"))
										(e-ident (raw "name"))))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "name")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "repro_issue_12942_unused_str_statement_not_interpolation_tie.Main.tool"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-tag (name "Ok")
				(args
					(e-lookup-local
						(p-assign (ident "name"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "repro_issue_12942_unused_str_statement_not_interpolation_tie.Main.served"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-tag (name "Err")
				(args
					(e-tag (name "Failed")
						(args
							(e-block
								(s-let
									(p-assign (ident "#interp_0"))
									(e-lookup-local
										(p-assign (ident "name"))))
								(s-let
									(p-assign (ident "#interp_1"))
									(e-lookup-local
										(p-assign (ident "name"))))
								(e-interpolation (constraint-fn-var 344) (dispatcher-var 38)
									(first
										(e-literal (string "")))
									(parts
										(e-lookup-local
											(p-assign (ident "#interp_0")))
										(e-literal (string ": "))
										(e-lookup-local
											(p-assign (ident "#interp_1")))
										(e-literal (string ""))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "repro_issue_12942_unused_str_statement_not_interpolation_tie.Main.realise"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 388)
									(e-lookup-local
										(p-assign (ident "repro_issue_12942_unused_str_statement_not_interpolation_tie.Main.served")))
									(e-lookup-local
										(p-assign (ident "name")))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-return
											(e-nominal-external
												(builtin)
												(e-tag (name "Err")
													(args
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))
				(s-expr
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "name")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-underscore)))))
	(s-nominal-decl
		(ty-header (name "Main"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Str, _a)"))
		(patt (type "Str -> Try(Str, [Failed(a)]) where [a.from_interpolation : List(Str) -> Try(Iter(Str) -> a, [InvalidInterpolation(Str)])]"))
		(patt (type "Str -> Try(Str, [Failed(a)]) where [a.from_interpolation : List(Str) -> Try(Iter(Str) -> a, [InvalidInterpolation(Str)])]")))
	(type_decls
		(nominal (type "Main")
			(ty-header (name "Main"))))
	(expressions
		(expr (type "Str -> Try(Str, _a)"))
		(expr (type "Str -> Try(Str, [Failed(a)]) where [a.from_interpolation : List(Str) -> Try(Iter(Str) -> a, [InvalidInterpolation(Str)])]"))
		(expr (type "Str -> Try(Str, [Failed(a)]) where [a.from_interpolation : List(Str) -> Try(Iter(Str) -> a, [InvalidInterpolation(Str)])]"))))
~~~
