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
── ✗ type mismatch ────────────── erroneous_scrutinee_pattern_consistency.md:1:5

The second branch of this match does not match the previous ones.

match undefined_scrutinee {
    { name } => name
    { name, age } => age
}

This second branch is trying to match:

    { age: _field, name: _field2 }

But the expression between the match parenthesis has the type:

    { name: _field }

These can never match! Either the pattern or expression has a problem.

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
