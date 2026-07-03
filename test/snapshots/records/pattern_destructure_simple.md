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
NAME NOT IN SCOPE - pattern_destructure_simple.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_simple.md:2:13:2:16
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `person` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  match person {                                                            │
 │        ‾‾‾‾‾‾                                                              │
 └───────────────────────────────────────── pattern_destructure_simple.md:1:7 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `age` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  { name, age } => name                                                     │
 │          ‾‾‾                                                               │
 └──────────────────────────────────────── pattern_destructure_simple.md:2:13 ┘

    If you don't need this variable, prefix it with an underscore like `_age`
    to suppress this warning.

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
