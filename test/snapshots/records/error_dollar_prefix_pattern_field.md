# META
~~~ini
description=Dollar-prefixed pattern record field names are rejected
type=expr
~~~
# SOURCE
~~~roc
match person {
    { $name } => $name
}
~~~
# EXPECTED
INVALID RECORD FIELD NAME - error_dollar_prefix_pattern_field.md:2:7:2:12
# PROBLEMS

┌───────────────────────────┐
│ INVALID RECORD FIELD NAME ├─ Record field names cannot start with a ────────┐
└┬──────────────────────────┘  dollar sign.                                   │
 │                                                                            │
 │  { $name } => $name                                                        │
 │    ‾‾‾‾‾                                                                   │
 └────────────────────────────────── error_dollar_prefix_pattern_field.md:2:7 ┘

    Names that start with `$` are reassignable variables declared with the
    `var` keyword, so they cannot be used as record field names.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
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
				(field (name "$name") (rest false)))
			(e-ident (raw "$name")))))
~~~
# FORMATTED
~~~roc
match person {
	{ $name } => $name
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
								(record-destruct (label "$name") (ident "$name")
									(required
										(p-assign (ident "$name"))))))))
				(value
					(e-lookup-local
						(p-assign (ident "$name"))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
