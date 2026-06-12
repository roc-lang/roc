# META
~~~ini
description=Nested record destructuring pattern in a match expression
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_nested.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_nested.md:2:38:2:45
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**pattern_destructure_nested.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNUSED VARIABLE**
Variable `zipCode` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_zipCode` to suppress this warning.
The unused variable is declared here:
**pattern_destructure_nested.md:2:38:2:45:**
```roc
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
```
                                     ^^^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "person"))
	(branches
		(branch
			(p-record
				(field (name "name") (rest false))
				(field (name "address") (rest false)
					(p-record
						(field (name "street") (rest false))
						(field (name "city") (rest false))
						(field (name "zipCode") (rest false)))))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "name"))
				(e-string-part (raw " lives on "))
				(e-ident (raw "street"))
				(e-string-part (raw " in "))
				(e-ident (raw "city"))
				(e-string-part (raw ""))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(required
										(p-assign (ident "name"))))
								(record-destruct (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "street") (ident "street")
													(required
														(p-assign (ident "street"))))
												(record-destruct (label "city") (ident "city")
													(required
														(p-assign (ident "city"))))
												(record-destruct (label "zipCode") (ident "zipCode")
													(required
														(p-assign (ident "zipCode"))))))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "name"))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-lookup-local
								(p-assign (ident "street"))))
						(s-let
							(p-assign (ident "#interp_2"))
							(e-lookup-local
								(p-assign (ident "city"))))
						(e-dispatch-call (method "from_interpolation") (constraint-fn-var 294)
							(receiver
								(e-string
									(e-literal (string ""))))
							(args
								(e-dispatch-call (method "prepended") (constraint-fn-var 252)
									(receiver
										(e-dispatch-call (method "prepended") (constraint-fn-var 194)
											(receiver
												(e-dispatch-call (method "prepended") (constraint-fn-var 136)
													(receiver
														(e-dispatch-call (method "iter") (constraint-fn-var 72)
															(receiver
																(e-empty_list))
															(args)))
													(args
														(e-tuple
															(elems
																(e-lookup-local
																	(p-assign (ident "#interp_2")))
																(e-string
																	(e-literal (string ""))))))))
											(args
												(e-tuple
													(elems
														(e-lookup-local
															(p-assign (ident "#interp_1")))
														(e-string
															(e-literal (string " in "))))))))
									(args
										(e-tuple
											(elems
												(e-lookup-local
													(p-assign (ident "#interp_0")))
												(e-string
													(e-literal (string " lives on ")))))))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
