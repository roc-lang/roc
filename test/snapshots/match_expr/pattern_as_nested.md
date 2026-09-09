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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 1) (end 4 2))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous ones."))
		(document
			(source-underlines
				(display (file "pattern_as_nested.md") (start 1 1) (end 4 2) (annotation dim) (line-text "match person {\n    { name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)\n    { name } as simplePerson => (simplePerson, name, \"unknown\")\n}"))
				(underline (start 3 5) (end 3 16) (annotation error)))
			(line-break)
			(reflow "This")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name: _field }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the expression between the")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "parenthesis has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ address: _field, name: _field2 }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This pattern doesn't bind the")
			(reflow " ")
			(annotated code "address")
			(reflow " ")
			(reflow "field. Match it explicitly with")
			(reflow " ")
			(annotated code "address: _")
			(reflow ",")
			(reflow " ")
			(reflow "or add")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "to match all the remaining fields."))))
~~~
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
