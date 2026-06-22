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
UNDEFINED VARIABLE - static_dispatch_super_test.md:1:1:1:8
UNDEFINED VARIABLE - static_dispatch_super_test.md:1:9:1:13
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:15
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:41
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:72
TRY OPERATOR OUTSIDE FUNCTION - static_dispatch_super_test.md:1:1:1:86
# PROBLEMS
                                                          ┌────────────────────┐
┌─ Nothing is named some_fn in this scope. ───────────────┤ UNDEFINED VARIABLE │
│                                                         └───────────────────┬┘
│                                                                             │
│  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?│
│  ‾‾‾‾‾‾‾                                                                    │
└─────────────────────────────────────────────────────────────────────────────┘
    static_dispatch_super_test.md:1:1

    Is there an import or exposing missing up-top?
                                                          ┌────────────────────┐
┌─ Nothing is named arg1 in this scope. ──────────────────┤ UNDEFINED VARIABLE │
│                                                         └───────────────────┬┘
│                                                                             │
│  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?│
│          ‾‾‾‾                                                               │
└─────────────────────────────────────────────────────────────────────────────┘
    static_dispatch_super_test.md:1:9

    Is there an import or exposing missing up-top?
                                               ┌───────────────────────────────┐
┌─ The ? operator can only be used inside ─────┤ TRY OPERATOR OUTSIDE FUNCTION │
│  function bodies because it can cause an     └──────────────────────────────┬┘
│  early return.                                                              │
│                                                                             │
│  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?│
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
    static_dispatch_super_test.md:1:1

                                               ┌───────────────────────────────┐
┌─ The ? operator can only be used inside ─────┤ TRY OPERATOR OUTSIDE FUNCTION │
│  function bodies because it can cause an     └──────────────────────────────┬┘
│  early return.                                                              │
│                                                                             │
│  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?│
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                   │
└─────────────────────────────────────────────────────────────────────────────┘
    static_dispatch_super_test.md:1:1

                                               ┌───────────────────────────────┐
┌─ The ? operator can only be used inside ─────┤ TRY OPERATOR OUTSIDE FUNCTION │
│  function bodies because it can cause an     └──────────────────────────────┬┘
│  early return.                                                              │
│                                                                             │
│  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?│
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾    │
└─────────────────────────────────────────────────────────────────────────────┘
    static_dispatch_super_test.md:1:1

                                               ┌───────────────────────────────┐
┌─ The ? operator can only be used inside ─────┤ TRY OPERATOR OUTSIDE FUNCTION │
│  function bodies because it can cause an     └──────────────────────────────┬┘
│  early return.                                                              │
│                                                                             │
│  some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?│
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾│
└─────────────────────────────────────────────────────────────────────────────┘
    static_dispatch_super_test.md:1:1

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
