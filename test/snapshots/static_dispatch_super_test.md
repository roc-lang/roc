# META
~~~ini
description=Dot access super test
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
~~~
# EXPECTED
NAME NOT IN SCOPE - static_dispatch_super_test.md:1:1:1:8
NAME NOT IN SCOPE - static_dispatch_super_test.md:1:9:1:13
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:15
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:41
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:72
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:86
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 1) (end 1 8))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "some_fn")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "static_dispatch_super_test.md") (start 1 1) (end 1 8) (annotation error) (line-text "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 9) (end 1 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "arg1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "static_dispatch_super_test.md") (start 1 9) (end 1 13) (annotation error) (line-text "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Try Operator Outside Function")
		(region (start 1 1) (end 1 15))
		(headline
			(reflow "The ")
			(annotated code "?")
			(reflow " operator can only be used inside function bodies because it can cause an early return."))
		(document
			(source-region (file "static_dispatch_super_test.md") (start 1 1) (end 1 15) (annotation error) (line-text "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Try Operator Outside Function")
		(region (start 1 1) (end 1 41))
		(headline
			(reflow "The ")
			(annotated code "?")
			(reflow " operator can only be used inside function bodies because it can cause an early return."))
		(document
			(source-region (file "static_dispatch_super_test.md") (start 1 1) (end 1 41) (annotation error) (line-text "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Try Operator Outside Function")
		(region (start 1 1) (end 1 72))
		(headline
			(reflow "The ")
			(annotated code "?")
			(reflow " operator can only be used inside function bodies because it can cause an early return."))
		(document
			(source-region (file "static_dispatch_super_test.md") (start 1 1) (end 1 72) (annotation error) (line-text "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Try Operator Outside Function")
		(region (start 1 1) (end 1 86))
		(headline
			(reflow "The ")
			(annotated code "?")
			(reflow " operator can only be used inside function bodies because it can cause an early return."))
		(document
			(source-region (file "static_dispatch_super_test.md") (start 1 1) (end 1 86) (annotation error) (line-text "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?")))))
~~~
# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-question-suffix
	(e-field-access
		(receiver
			(e-question-suffix
				(e-method-call (method ".next_static_dispatch_method")
					(receiver
						(e-question-suffix
							(e-method-call (method ".static_dispatch_method")
								(receiver
									(e-question-suffix
										(e-apply
											(e-ident (raw "some_fn"))
											(e-ident (raw "arg1")))))
								(args))))
					(args))))
		(segment (mode "required") (field "record_field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-field-access
				(receiver
					(e-match
						(match
							(cond
								(e-method-call (method "next_static_dispatch_method")
									(receiver
										(e-match
											(match
												(cond
													(e-method-call (method "static_dispatch_method")
														(receiver
															(e-match
																(match
																	(cond
																		(e-call
																			(e-runtime-error (tag "ident_not_in_scope"))
																			(e-runtime-error (tag "ident_not_in_scope"))))
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
																				(e-runtime-error (tag "return_outside_fn"))))))))
														(args)))
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
															(e-runtime-error (tag "return_outside_fn"))))))))
									(args)))
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
										(e-runtime-error (tag "return_outside_fn"))))))))
				(segments
					(segment (name "record_field") (mode "required")))))
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
					(e-runtime-error (tag "return_outside_fn")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
