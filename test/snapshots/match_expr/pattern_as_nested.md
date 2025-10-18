# META
~~~ini
description=Nested as patterns with tuples and records
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)
    { name } as simplePerson => (simplePerson, name, "unknown")
}
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_as_nested.md:1:7:1:13
UNUSED VARIABLE - pattern_as_nested.md:2:7:2:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**pattern_as_nested.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNUSED VARIABLE**
Variable `name` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**pattern_as_nested.md:2:7:2:11:**
```roc
    { name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)
```
      ^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,KwAs,LowerIdent,CloseCurly,KwAs,LowerIdent,OpFatArrow,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
OpenCurly,LowerIdent,CloseCurly,KwAs,LowerIdent,OpFatArrow,OpenRound,LowerIdent,Comma,LowerIdent,Comma,StringStart,StringPart,StringEnd,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "person"))
	(branches
		(branch
			(p-as (name "fullPerson")
				(p-record
					(field (name "name") (rest false))
					(field (name "address") (rest false)
						(p-as (name "addr")
							(p-record
								(field (name "city") (rest false)))))))
			(e-tuple
				(e-ident (raw "fullPerson"))
				(e-ident (raw "addr"))
				(e-ident (raw "city"))))
		(branch
			(p-as (name "simplePerson")
				(p-record
					(field (name "name") (rest false))))
			(e-tuple
				(e-ident (raw "simplePerson"))
				(e-ident (raw "name"))
				(e-string
					(e-string-part (raw "unknown")))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)
	{ name } as simplePerson => (simplePerson, name, "unknown")
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
						(p-as (as "fullPerson")
							(p-record-destructure
								(destructs
									(record-destruct (label "name") (ident "name")
										(required
											(p-assign (ident "name"))))
									(record-destruct (label "address") (ident "address")
										(sub-pattern
											(p-as (as "addr")
												(p-record-destructure
													(destructs
														(record-destruct (label "city") (ident "city")
															(required
																(p-assign (ident "city"))))))))))))))
				(value
					(e-tuple
						(elems
							(e-lookup-local
								(p-as (as "fullPerson")
									(p-record-destructure
										(destructs
											(record-destruct (label "name") (ident "name")
												(required
													(p-assign (ident "name"))))
											(record-destruct (label "address") (ident "address")
												(sub-pattern
													(p-as (as "addr")
														(p-record-destructure
															(destructs
																(record-destruct (label "city") (ident "city")
																	(required
																		(p-assign (ident "city")))))))))))))
							(e-lookup-local
								(p-as (as "addr")
									(p-record-destructure
										(destructs
											(record-destruct (label "city") (ident "city")
												(required
													(p-assign (ident "city"))))))))
							(e-lookup-local
								(p-assign (ident "city")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-as (as "simplePerson")
							(p-record-destructure
								(destructs
									(record-destruct (label "name") (ident "name")
										(required
											(p-assign (ident "name")))))))))
				(value
					(e-tuple
						(elems
							(e-lookup-local
								(p-as (as "simplePerson")
									(p-record-destructure
										(destructs
											(record-destruct (label "name") (ident "name")
												(required
													(p-assign (ident "name"))))))))
							(e-lookup-local
								(p-assign (ident "name")))
							(e-string
								(e-literal (string "unknown"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "(Error, Error, Str)"))
~~~
