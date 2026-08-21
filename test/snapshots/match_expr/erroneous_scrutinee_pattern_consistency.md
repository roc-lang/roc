# META
~~~ini
description=Branch patterns stay mutually consistent when the scrutinee is erroneous
type=expr
~~~
# SOURCE
~~~roc
match undefined_scrutinee {
    { name } => name
    { name, age } => age
}
~~~
# EXPECTED
TYPE MISMATCH - erroneous_scrutinee_pattern_consistency.md:1:1:1:1
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
				(display (file "erroneous_scrutinee_pattern_consistency.md") (start 1 1) (end 4 2) (annotation dim) (line-text "match undefined_scrutinee {\n    { name } => name\n    { name, age } => age\n}"))
				(underline (start 3 5) (end 3 18) (annotation error)))
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
			(text "{ age: _field, name: _field2 }")
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
			(text "{ name: _field }")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem."))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "undefined_scrutinee"))
	(branches
		(branch
			(p-record
				(field (name "name") (rest false)))
			(e-ident (raw "name")))
		(branch
			(p-record
				(field (name "name") (rest false))
				(field (name "age") (rest false)))
			(e-ident (raw "age")))))
~~~
# FORMATTED
~~~roc
match undefined_scrutinee {
	{ name } => name
	{ name, age } => age
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
										(p-assign (ident "name"))))))))
				(value
					(e-lookup-local
						(p-assign (ident "name")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(required
										(p-assign (ident "name"))))
								(record-destruct (label "age") (ident "age")
									(required
										(p-assign (ident "age"))))))))
				(value
					(e-lookup-local
						(p-assign (ident "age"))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
