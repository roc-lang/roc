# META
~~~ini
description=Simple record destructuring in match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name } => name
    { age } => age
}
~~~
# EXPECTED
REDUNDANT PATTERN - simple_record.md:1:1:4:2
# PROBLEMS
**REDUNDANT PATTERN**
The second branch of this `match` is redundant:
**simple_record.md:1:1:4:2:**
```roc
match person {
    { name } => name
    { age } => age
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
OpenCurly,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
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
				(field (name "name") (rest false)))
			(e-ident (raw "name")))
		(branch
			(p-record
				(field (name "age") (rest false)))
			(e-ident (raw "age")))))
~~~
# FORMATTED
~~~roc
match person {
	{ name } => name
	{ age } => age
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-lookup-local
				(p-assign (ident "person"))))
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
