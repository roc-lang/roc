# META
~~~ini
description=A dollar-prefixed punned record pattern is an immutable binding
type=expr
~~~
# SOURCE
~~~roc
match person {
    { $name } => $name
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
