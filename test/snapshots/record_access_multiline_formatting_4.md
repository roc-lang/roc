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
UNDEFINED VARIABLE - record_access_multiline_formatting_4.md:1:1:1:8
UNDEFINED VARIABLE - record_access_multiline_formatting_4.md:1:9:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `some_fn` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_access_multiline_formatting_4.md:1:1:1:8:**
```roc
some_fn(arg1)? # Comment 1
```
^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `arg1` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_access_multiline_formatting_4.md:1:9:1:13:**
```roc
some_fn(arg1)? # Comment 1
```
        ^^^^


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
(e-field-access
	(e-field-access
		(e-field-access
			(e-question-suffix
				(e-apply
					(e-ident (raw "some_fn"))
					(e-ident (raw "arg1"))))
			(e-question-suffix
				(e-apply
					(e-ident (raw ".static_dispatch_method")))))
		(e-question-suffix
			(e-apply
				(e-ident (raw ".next_static_dispatch_method")))))
	(e-question-suffix
		(e-ident (raw ".record_field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "unknown")
	(receiver
		(e-dot-access (field "unknown")
			(receiver
				(e-dot-access (field "unknown")
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
												(p-applied-tag)))
										(value
											(e-lookup-local
												(p-assign (ident "#ok")))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-return
												(e-tag (name "Err")
													(args
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
