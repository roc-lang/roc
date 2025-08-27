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
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:20),OpColon(2:20-2:21),OpenCurly(2:22-2:23),LowerIdent(2:24-2:28),CloseCurly(2:29-2:30),KwAs(2:31-2:33),LowerIdent(2:34-2:38),CloseCurly(2:39-2:40),KwAs(2:41-2:43),LowerIdent(2:44-2:54),OpFatArrow(2:55-2:57),OpenRound(2:58-2:59),LowerIdent(2:59-2:69),Comma(2:69-2:70),LowerIdent(2:71-2:75),Comma(2:75-2:76),LowerIdent(2:77-2:81),CloseRound(2:81-2:82),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:11),CloseCurly(3:12-3:13),KwAs(3:14-3:16),LowerIdent(3:17-3:29),OpFatArrow(3:30-3:32),OpenRound(3:33-3:34),LowerIdent(3:34-3:46),Comma(3:46-3:47),LowerIdent(3:48-3:52),Comma(3:52-3:53),StringStart(3:54-3:55),StringPart(3:55-3:62),StringEnd(3:62-3:63),CloseRound(3:63-3:64),
CloseCurly(4:1-4:2),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "person"))
	(branches
		(branch @2.5-2.82
			(p-as @2.5-2.43 (name "fullPerson")
				(p-record @2.5-2.40
					(field @2.7-2.11 (name "name") (rest false))
					(field @2.13-2.38 (name "address") (rest false)
						(p-as @2.22-2.33 (name "addr")
							(p-record @2.22-2.30
								(field @2.24-2.28 (name "city") (rest false)))))))
			(e-tuple @2.58-2.82
				(e-ident @2.59-2.69 (raw "fullPerson"))
				(e-ident @2.71-2.75 (raw "addr"))
				(e-ident @2.77-2.81 (raw "city"))))
		(branch @3.5-3.64
			(p-as @3.5-3.16 (name "simplePerson")
				(p-record @3.5-3.13
					(field @3.7-3.11 (name "name") (rest false))))
			(e-tuple @3.33-3.64
				(e-ident @3.34-3.46 (raw "simplePerson"))
				(e-ident @3.48-3.52 (raw "name"))
				(e-string @3.54-3.63
					(e-string-part @3.55-3.62 (raw "unknown")))))))
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
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-as @2.5-2.43 (as "fullPerson")
							(p-record-destructure @2.5-2.40
								(destructs
									(record-destruct @2.7-2.11 (label "name") (ident "name")
										(required
											(p-assign @2.7-2.11 (ident "name"))))
									(record-destruct @2.13-2.38 (label "address") (ident "address")
										(sub-pattern
											(p-as @2.22-2.33 (as "addr")
												(p-record-destructure @2.22-2.30
													(destructs
														(record-destruct @2.24-2.28 (label "city") (ident "city")
															(required
																(p-assign @2.24-2.28 (ident "city"))))))))))))))
				(value
					(e-tuple @2.58-2.82
						(elems
							(e-lookup-local @2.59-2.69
								(p-as @2.5-2.43 (as "fullPerson")
									(p-record-destructure @2.5-2.40
										(destructs
											(record-destruct @2.7-2.11 (label "name") (ident "name")
												(required
													(p-assign @2.7-2.11 (ident "name"))))
											(record-destruct @2.13-2.38 (label "address") (ident "address")
												(sub-pattern
													(p-as @2.22-2.33 (as "addr")
														(p-record-destructure @2.22-2.30
															(destructs
																(record-destruct @2.24-2.28 (label "city") (ident "city")
																	(required
																		(p-assign @2.24-2.28 (ident "city")))))))))))))
							(e-lookup-local @2.71-2.75
								(p-as @2.22-2.33 (as "addr")
									(p-record-destructure @2.22-2.30
										(destructs
											(record-destruct @2.24-2.28 (label "city") (ident "city")
												(required
													(p-assign @2.24-2.28 (ident "city"))))))))
							(e-lookup-local @2.77-2.81
								(p-assign @2.24-2.28 (ident "city")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-as @3.5-3.16 (as "simplePerson")
							(p-record-destructure @3.5-3.13
								(destructs
									(record-destruct @3.7-3.11 (label "name") (ident "name")
										(required
											(p-assign @3.7-3.11 (ident "name")))))))))
				(value
					(e-tuple @3.33-3.64
						(elems
							(e-lookup-local @3.34-3.46
								(p-as @3.5-3.16 (as "simplePerson")
									(p-record-destructure @3.5-3.13
										(destructs
											(record-destruct @3.7-3.11 (label "name") (ident "name")
												(required
													(p-assign @3.7-3.11 (ident "name"))))))))
							(e-lookup-local @3.48-3.52
								(p-assign @3.7-3.11 (ident "name")))
							(e-string @3.54-3.63
								(e-literal @3.55-3.62 (string "unknown"))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "(Error, _field, Str)"))
~~~
