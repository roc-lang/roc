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
TYPE MISMATCH - pattern_as_nested.md:1:1:1:1
# PROBLEMS
── ✗ type mismatch ──────────────────────────────────── pattern_as_nested.md:1:5

The second branch of this match does not match the previous ones.

match person {
    { name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)
    { name } as simplePerson => (simplePerson, name, "unknown")
}

This second branch is trying to match:

    { name: _field }

But the expression between the match parenthesis has the type:

    { address: _field, name: _field2 }

These can never match! Either the pattern or expression has a problem.
Hint: This pattern doesn't bind the address field. Match it explicitly with
address: _, or add .. to match all the remaining fields.

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
(expr (type "({ address: { city: a }, name: _field }, { city: a }, a)"))
~~~
