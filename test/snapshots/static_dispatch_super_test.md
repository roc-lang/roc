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

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `some_fn` in this scope. ─────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.… │
 │  ‾‾‾‾‾‾‾                                                                   │
 └───────────────────────────────────────── static_dispatch_super_test.md:1:1 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `arg1` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.… │
 │          ‾‾‾‾                                                              │
 └───────────────────────────────────────── static_dispatch_super_test.md:1:9 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────────────────┐
│ TRY OPERATOR OUTSIDE FUNCTION ├─ The `?` operator can only be used inside ──┐
└┬──────────────────────────────┘  function bodies because it can cause an    │
 │                                 early return.                              │
 │                                                                            │
 │  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.… │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                            │
 └───────────────────────────────────────── static_dispatch_super_test.md:1:1 ┘



┌───────────────────────────────┐
│ TRY OPERATOR OUTSIDE FUNCTION ├─ The `?` operator can only be used inside ──┐
└┬──────────────────────────────┘  function bodies because it can cause an    │
 │                                 early return.                              │
 │                                                                            │
 │  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.… │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                  │
 └───────────────────────────────────────── static_dispatch_super_test.md:1:1 ┘



┌───────────────────────────────┐
│ TRY OPERATOR OUTSIDE FUNCTION ├─ The `?` operator can only be used inside ──┐
└┬──────────────────────────────┘  function bodies because it can cause an    │
 │                                 early return.                              │
 │                                                                            │
 │  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.… │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾   │
 └───────────────────────────────────────── static_dispatch_super_test.md:1:1 ┘



┌───────────────────────────────┐
│ TRY OPERATOR OUTSIDE FUNCTION ├─ The `?` operator can only be used inside ──┐
└┬──────────────────────────────┘  function bodies because it can cause an    │
 │                                 early return.                              │
 │                                                                            │
 │  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.… │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  │
 └───────────────────────────────────────── static_dispatch_super_test.md:1:1 ┘


# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-question-suffix
	(e-field-access
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
				(args)))
		(e-ident (raw "record_field"))))
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
			(e-field-access (field "record_field")
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
										(e-runtime-error (tag "return_outside_fn"))))))))))
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
