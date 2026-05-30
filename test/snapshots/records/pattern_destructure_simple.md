# META
~~~ini
description=Simple record destructuring pattern
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# EXPECTED
UNUSED VARIABLE - pattern_destructure_simple.md:2:13:2:16
# PROBLEMS
**UNUSED VARIABLE**
Variable `age` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_age` to suppress this warning.
The unused variable is declared here:
**pattern_destructure_simple.md:2:13:2:16:**
```roc
    { name, age } => name
```
            ^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
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
				(field (name "age") (rest false)))
			(e-ident (raw "name")))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, age } => name
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
										(p-assign (ident "name"))))
								(record-destruct (label "age") (ident "age")
									(required
										(p-assign (ident "age"))))))))
				(value
					(e-lookup-local
						(p-assign (ident "name"))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
