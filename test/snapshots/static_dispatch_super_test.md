# META
~~~ini
description=Dot access super test
type=expr
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
**UNDEFINED VARIABLE**
Nothing is named `some_fn` in this scope.
Is there an `import` or `exposing` missing up-top?

**static_dispatch_super_test.md:1:1:1:8:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `arg1` in this scope.
Is there an `import` or `exposing` missing up-top?

**static_dispatch_super_test.md:1:9:1:13:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
        ^^^^


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**static_dispatch_super_test.md:1:1:1:15:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
^^^^^^^^^^^^^^


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**static_dispatch_super_test.md:1:1:1:41:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**static_dispatch_super_test.md:1:1:1:72:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**static_dispatch_super_test.md:1:1:1:86:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


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
			(e-field-access
				(e-question-suffix
					(e-field-access
						(e-question-suffix
							(e-apply
								(e-ident (raw "some_fn"))
								(e-ident (raw "arg1"))))
						(e-apply
							(e-ident (raw "static_dispatch_method")))))
				(e-apply
					(e-ident (raw "next_static_dispatch_method")))))
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
			(e-dot-access (field "record_field")
				(receiver
					(e-match
						(match
							(cond
								(e-dot-access (field "next_static_dispatch_method")
									(receiver
										(e-match
											(match
												(cond
													(e-dot-access (field "static_dispatch_method")
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
