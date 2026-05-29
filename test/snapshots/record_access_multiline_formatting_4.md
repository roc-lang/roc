# META
~~~ini
description=record_access_multiline_formatting (4)
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
	.next_static_dispatch_method()? # Comment 3
	.record_field?
~~~
# EXPECTED
TRY OPERATOR OUTSIDE FUNCTION - record_access_multiline_formatting_4.md:1:1:1:15
TRY OPERATOR OUTSIDE FUNCTION - record_access_multiline_formatting_4.md:1:1:2:28
TRY OPERATOR OUTSIDE FUNCTION - record_access_multiline_formatting_4.md:1:1:3:33
TRY OPERATOR OUTSIDE FUNCTION - record_access_multiline_formatting_4.md:1:1:4:16
# PROBLEMS
**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**record_access_multiline_formatting_4.md:1:1:1:15:**
```roc
some_fn(arg1)? # Comment 1
```
^^^^^^^^^^^^^^


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**record_access_multiline_formatting_4.md:1:1:2:28:**
```roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
```


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**record_access_multiline_formatting_4.md:1:1:3:33:**
```roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
	.next_static_dispatch_method()? # Comment 3
```


**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**record_access_multiline_formatting_4.md:1:1:4:16:**
```roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
	.next_static_dispatch_method()? # Comment 3
	.record_field?
```


# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
DotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,
DotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,
DotLowerIdent,NoSpaceOpQuestion,
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
							(e-ident (raw ".static_dispatch_method")))))
				(e-apply
					(e-ident (raw ".next_static_dispatch_method")))))
		(e-ident (raw ".record_field"))))
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
																			(e-lookup-local
																				(p-assign (ident "some_fn")))
																			(e-lookup-local
																				(p-assign (ident "arg1")))))
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
